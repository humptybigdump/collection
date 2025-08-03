#!/usr/bin/env python


import gzip
import png
import struct
import numpy as np


try:
    # For Python 3.0 and later
    from urllib.request import urlopen
except ImportError:
    # Fall back to Python 2's urllib2
    from urllib2 import urlopen


def download_and_extract(urls, tag):
    '''
    Download the dataset as gzip file and extract them.

    Args:
        urls: Dictionary with urls
        tag: Tag used for writing file

    Returns:
        output_files: Dictionary with output filenames
    '''

    output_files = {}
    for key in urls:
        # Download file
        filename_output = '{}_{}.gz'.format(tag, key)
        print('Download file from url: {} -> {}'.format(urls[key], filename_output))
        response = urlopen(urls[key])
        file_ = response.read()

        with open(filename_output, 'wb') as f:
            f.write(file_)

        # Extract gzip files
        with gzip.open(filename_output, 'rb') as f:
            unzipped = f.read()
            filename_unzipped = filename_output.replace('.gz', '.bin')
            output_files[key] = filename_unzipped
            print('Unzip file: {} -> {}'.format(filename_output, filename_unzipped))
            with open(filename_unzipped, 'wb') as u:
                u.write(unzipped)

    return output_files


def load_data(filename_images, filename_labels, verbose=True):
    '''
    Load images and labels from binary files

    Args:
        filename_images: Filename of binary file with image data
        filename_labels: Filename of binary file with label data

    Returns:
        images: Array with images of size (num_images,28,28)
        labels: Array with labesl of size (num_images)
    '''

    # Get images
    with open(filename_images, 'rb') as f:
        magic, num_images, rows, cols = struct.unpack(">IIII", f.read(16))
        images = np.fromfile(f, dtype=np.uint8).reshape(num_images, rows, cols)

    # Get labels
    with open(filename_labels, 'rb') as f:
        magic, num_labels = struct.unpack(">II", f.read(8))
        labels = np.fromfile(f, dtype=np.int8)

    # Convert from uint8 to float32
    images = images.astype('float32')
    labels = labels.astype('float32')

    # Expand dimensions of images because Keras expects images
    # of shape (height, width, channels) but a greyscale image can
    # be represented by a matrix with shape (height, width).
    images = np.expand_dims(images, axis=-1)

    if verbose:
        print('Loaded {} images and labels from files {} and {}.'.format(
                num_images, filename_images, filename_labels))

    return images, labels


def binary_to_png(files, num_images):
    '''
    Extract some images from binary file and convert them to PNG.

    Args:
        files: Filenames of images and labels
        num_images: Number of images which are extracted and converted
    '''

    # Load images and labels
    images, labels = load_data(files['images'], files['labels'])

    # Files are in greyscale (shape 28x28 with 1 color channel)
    w = png.Writer(28, 28, greyscale=True)

    # Write num_images as PNG files to disk
    for i_image, (image, label) in enumerate(zip(images[:num_images], labels[:num_images])):
        w.write(open('example_input_{}_digit_{}.png'.format(
                i_image, int(label)), 'wb'), image)
    print('Wrote {} images to disk.'.format(num_images))


if __name__ == '__main__':
    # Set urls to download MNIST dataset

    # Official website
    urls_train = {
            'images' : 'http://yann.lecun.com/exdb/mnist/train-images-idx3-ubyte.gz',
            'labels' : 'http://yann.lecun.com/exdb/mnist/train-labels-idx1-ubyte.gz'
            }
    urls_test= {
            'images' : 'http://yann.lecun.com/exdb/mnist/t10k-images-idx3-ubyte.gz',
            'labels' : 'http://yann.lecun.com/exdb/mnist/t10k-labels-idx1-ubyte.gz'
            }

    # Download and extract the dataset
    # NOTE: The downloaded files are images with shape 28x28 in a highly compressed
    # format. More information here: http://yann.lecun.com/exdb/mnist/
    files_train = download_and_extract(urls_train, 'train')
    files_test = download_and_extract(urls_test, 'test')

    # Transform some images from the binaries to PNG images so that you can see
    # the inputs.
    binary_to_png(files_train, 20)
