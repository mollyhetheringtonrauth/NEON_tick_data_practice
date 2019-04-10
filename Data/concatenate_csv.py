#! /usr/bin/env python

import zipfile
import os
import glob
import re
import shutil
import csv


def get_csv_data_into_tmp_tick_count ():
    '''
    unzips each zipped directory within your working directory
    copies the csv file with desired data into a tmp directory
      '''
    # create tempory directory to compile all csv files in
    if os.path.exists('tmp_fielddata'):
        shutil.rmtree('tmp_fielddata')
    if os.path.exists('tmp_taxonomyProcessed'):
        shutil.rmtree('tmp_taxonomyProcessed')
    os.mkdir('tmp_fielddata')
    os.mkdir('tmp_taxonomyProcessed')

    # unzip directory and copy desired csv file into tmp directory
    for zipped_folder in glob.glob('*.zip'):
        directory_name = zipped_folder.split('.zip',)[0]
        with zipfile.ZipFile(zipped_folder) as zip_ref:
            zip_ref.extractall(directory_name)

        # move into unzipped directory
        os.chdir(directory_name)

        # find, copy, and paste desired csv file into tmp directory
        for file in os.listdir(os.getcwd()):
            if re.match('.*tck_fielddata.*', file):
                shutil.copy(file, '../tmp_fielddata')
            if re.match('.*tck_taxonomyProcessed.*', file):
                shutil.copy(file, '../tmp_taxonomyProcessed')

        # move out of unzipped directory and then delete unzipped directory
        os.chdir('..')
        shutil.rmtree(directory_name)

def concatenate_csv_files ():
    '''
    takes a directory with csv files you wish to concantenate and
    creates a csv file called '../concatenated.csv'

    the '../concatenated.csv' file contains all the rows of data from each
    csv file within the directory

    the csv files you wish to concatenate must all have the same headers

    '''

    # get the tmp directory name that you will have moved into
    tmp_name = os.getcwd().split('/')[-1]

    # list csv files in the given directory
    csv_files_in_dir = glob.glob('*.csv')
    example_csv = csv_files_in_dir[0]

    # determine the header of the csv files
    with open(example_csv, 'r') as csv_file:
        csv_reader = csv.reader(csv_file)
        header = next(csv_reader)
        # print (header)

    # create a concatenated.csv and add the header
    concatFileName = '../' + tmp_name.split('_')[1] + '_concatenated.csv'
    with open (concatFileName, 'w') as csv_out:
        csv_writer =csv.writer(csv_out, delimiter =',')
        csv_writer.writerow(header)

    # remove header from csv file, append each row in csv file to concatenated.csv
    for csv_file in csv_files_in_dir:
        with open (csv_file, 'r') as csv_file:
            csv_reader = csv.reader(csv_file)
            header_match = next(csv_reader)

            if header_match == header:
                with open (concatFileName, 'a') as csv_out:
                    csv_writer = csv.writer(csv_out, delimiter = ',')

                    for line in csv_reader:
                        csv_writer.writerow(line)



if __name__=="__main__":

    # unzips directories and moves csv files into a tmp directory
    get_csv_data_into_tmp_tick_count ()

    # move into the tmp directory created by get_csv_data_into_tmp_tick_count()
    # concatentate csv files for fielddata
    os.chdir('tmp_fielddata')
    concatenate_csv_files()
    os.chdir('..')
    shutil.rmtree('tmp_fielddata')

    # move into the tmp directory created by get_csv_data_into_tmp_tick_count()
    # concatentate csv files for taxonomy
    os.chdir('tmp_taxonomyProcessed')
    concatenate_csv_files()
    os.chdir('..')
    shutil.rmtree('tmp_taxonomyProcessed')

    print ('DONE!')
