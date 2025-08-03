import pandas as pd
import re
import flair
from datetime import datetime
import os
from striprtf.striprtf import rtf_to_text


def create_log(filename):
    """
    Creates a logfile.
    :param filename: name of the logfile (Str)
    :return: None
    """
    with open(filename, 'w') as file:
        timestamp = datetime.now()
        file.write(str(timestamp) + ': Process started')


def write_log(msg, logfile):
    """
    appends the given message to the given logfile
    :param msg: message to append (Str)
    :param logfile: name of the logfile (Str)
    :return: None
    """
    if logfile is not None:
        with open(logfile, 'a') as file:
            file.write('\n')
            file.write(msg)


def print_progress_bar(iteration, total, prefix='', suffix='', decimals=1, length=50, fill='█', print_end=""):
    """
    Call in a loop to create terminal progress bar
    @params:
        iteration   - Required  : current iteration (Int)
        total       - Required  : total iterations (Int)
        prefix      - Optional  : prefix string (Str)
        suffix      - Optional  : suffix string (Str)
        decimals    - Optional  : positive number of decimals in percent complete (Int)
        length      - Optional  : character length of bar (Int)
        fill        - Optional  : bar fill character (Str)
        printEnd    - Optional  : end character (e.g. "\r", "\r\n") (Str)
    """
    percent = ("{0:." + str(decimals) + "f}").format(100 * (iteration / float(total)))
    filled_length = int(length * iteration // total)
    bar = fill * filled_length + '-' * (length - filled_length)
    print(f'\r{prefix} |{bar}| {percent}% {suffix}', end=print_end)
    # Print New Line on Complete
    if iteration == total:
        print()


def read_articles(filename, logfile):
    """
    Read text file with articles from LexisNexis, split them into single documents using document structure
    :param filename: name of the text file with the articles (Str)
    :param logfile:  name of the logfile created by the script (Str)
    :return: Pandas DataFrame with one column (content) containing the articles
    """
    try:
        with open(filename, "r", encoding="utf-8") as file:
            documents = file.read()
    except UnicodeDecodeError:
        with open(filename, "r", encoding="ANSI") as file:
            documents = file.read()
    documents = rtf_to_text(documents)
    documents = documents.split('End of Document')[:-1]
    write_log(f"{datetime.now()}: Read file {filename}. Found {len(documents)} articles.", logfile)
    print(f"Found {len(documents)} articles.")
    documents_dataframe = pd.DataFrame(documents, columns=["content"])
    return documents_dataframe


def clean_articles(documents, logfile):
    """
    Clean articles from LexisNexis and extract headline, article body, complete text and article length.
    Uses document structure to find body etc.
    :param documents: Pandas DataFrame with the articles. Must contain column "contents" (pandas.DataFrame)
    :param logfile: name of the logfile created by the script (Str)
    :return: Pandas DataFrame with new columns for the cleaned content ("content_clean"), title, source, pubdate,
    article length ("length_article"), article body ("body") and full text ("complete_text").
    """
    documents["content_clean"] = documents.content.apply(
        lambda x: re.sub("\xa0", " ", re.sub(r"\ufeff", " ", x)).strip())
    documents.content_clean = documents.content_clean.apply(
        lambda x: re.sub(r"\n{2,}", "\n", re.sub(r" \n", "\n", re.sub(r" {2,}", " ", x))))
    matches = documents.content_clean.apply(
        lambda x: re.search(r"([^\n]*)\n+([^\n]*)\n+([^\n]*)\n+", x)
    )
    documents["title"] = [match.group(1) for match in matches]
    documents["source"] = [match.group(2) for match in matches]
    documents["pubdate"] = [match.group(3) for match in matches]
    documents["length_article"] = documents.content_clean.apply(
        lambda x: int(re.search(r"\nLength: (\d+) ", x).group(1)))

    def read_body(document):
        """
        Find and extract article body in LexisNexis documents.
        :param document: raw text of article from LexisNexis (Str)
        :return: Part of the text that contains the article body (Str)
        """
        try:
            if re.search(r'\sOriginal Gesamtseiten-PDF', document):
                end_body = re.search(r'\sOriginal Gesamtseiten-PDF', document).start()
            elif re.search(r'\sGraphic\s', document):
                end_body = re.search(r'\sGraphic\s', document).start()
            else:
                end_body = re.search(r'\sLoad-Date', document).start()
            if re.search(r"\sPDF-Datei dieses Dokuments", document):
                start_body = re.search(r"\sPDF-Datei dieses Dokuments", document).end()
            elif re.search(r"\sBody", document):
                start_body = re.search(r"\sBody", document).end()
            else:
                start_body = re.search(r"\sByline", document).end()
            document_body = re.sub(r"\s+", " ", document[start_body:end_body]).strip()
        except AttributeError:
            document_body = "Fehler beim Auslesen des Inhalts"
        return document_body

    def read_byline(document):
        if re.search(r"\sByline:", document):
            if re.search(r"\sHighlight:", document):
                return re.search(r"\sByline: ([\s\S]*?)Highlight:", document).group(1).strip()
            else:
                return re.search(r"\sByline: ([\s\S]*?)Body", document).group(1).strip()
        else:
            return "Nicht angegeben"

    def read_section(document):
        if re.search(r"\sSection: ", document):
            return re.search(r"\sSection:\s([^;\n]*)", document).group(1)
        else:
            return "Nicht angegeben"

    documents["body"] = documents.content_clean.apply(read_body)
    documents["byline"] = documents.content_clean.apply(read_byline)
    documents["section"] = documents.content_clean.apply(read_section)
    unsuccessful_cases = sum(documents['body'] == 'Fehler beim Auslesen des Inhalts')
    write_log(f"{datetime.now()}: Cleaned all articles. Was unsuccessful in {unsuccessful_cases} cases.", logfile)
    print("Cleaned articles.")
    documents["complete_text"] = documents["title"] + " " + documents["body"]
    return documents

if __name__ == '__main__':
    logfile = os.path.join('log', input('Name of the Logfile?'))
    create_log(logfile)
    dataset_name = input('Name of the file with the documents?')
    articles_dataframe = read_articles(os.path.join('daten', dataset_name), logfile)
    articles_dataframe = clean_articles(articles_dataframe, logfile)
    all_articles = pd.DataFrame(articles_dataframe)
    
    new_csv_file = f"documents_from_{dataset_name[:-3]}csv"
    all_articles[
        ["title",
         "source",
         "pubdate",
         "length_article",
         "body",
         "byline",
         "section"]
    ].to_csv(os.path.join("daten", new_csv_file), sep=",", index=False, encoding="UTF-8")
    write_log(f"{datetime.now()}: Created file {new_csv_file} containing all identified documents", logfile)
    print(f"Created file {new_csv_file} containing all identified documents.")
    write_log(f"{datetime.now()}: Process terminated.", logfile)
    input('\nPress Enter to exit.')
