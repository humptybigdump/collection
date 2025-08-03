def create_dict_and_append_key_value_pair(key, value, newdict=None):
    """
    Add a key-value pair (`key`, `value`) to the dictionary `newdict`. 
    If no dictionary is provided, a new dictionary is created.

    Parameters:
    key (any): The key to add to the dictionary.
    value (any): The value to associate with the key in the dictionary.
    newdict (dict, optional): The dictionary to add the key-value pair to. Defaults to None, in which case a new dictionary is created.

    Returns:
    dict: The dictionary with the added key-value pair.
    """
    if newdict is None:
        newdict = {}  # Create a new dictionary if none is provided
    newdict[key] = value
    return newdict

meine_orangen = create_dict_and_append_key_value_pair('oranges', 2)
print(meine_orangen)

meine_aepfel = create_dict_and_append_key_value_pair('apples', 5)
print(meine_aepfel)

