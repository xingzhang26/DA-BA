<?php

namespace Combiner;

use Exception;
use League\Csv\Reader;
use League\Csv\Writer;
use SplTempFileObject;
use Symfony\Component\Console\Command\Command;
use Symfony\Component\Console\Input\InputArgument;
use Symfony\Component\Console\Input\InputInterface;
use Symfony\Component\Console\Output\OutputInterface;

class CsvCombiner extends Command
{
    protected function configure()
    {
        $this
            ->setName('csv')
            ->setDescription('PMG Coding')
            ->addArgument(
                'files',
                InputArgument::IS_ARRAY | InputArgument::REQUIRED,
                'CSV files to combine'
            );
    }


    protected function execute(InputInterface $input, OutputInterface $output)
    {
        try {
            $files = $this->getFileListFromArguments($input);

            $newFile = $this->newCsvFile();

            foreach($files as $filepath) {
                $newFile = $this->moveContentsToNewFile($filepath, $newFile);
            }

            $newFile->output();

        } catch(Exception $exception) {
            $output->writeln('<error>' . $exception->getCode(). ' ' . $exception->getMessage() . '</error>');
        }
    }

    
    protected function getFileListFromArguments(InputInterface $input)
    {
        $files = $input->getArgument('files');

        foreach($files as $filepath) {
            if( ! file_exists($filepath)) {
                throw new Exception('Invalid file: ' . $filepath);
            }
        }

        return $files;
    }


    protected function newCsvFile()
    {
        $newFile = Writer::createFromFileObject(new SplTempFileObject());
        $newFile->insertOne(['email_hash', 'category', 'filename']);

        return $newFile;
    }


    protected function importAndReadFile($filepath)
    {
        $file = Reader::createFromPath($filepath);
        $results = $file->fetchAssoc(0);

        return $results;
    }


    protected function moveContentsToNewFile($filepath, Writer $newFile)
    {
        $file = $this->importAndReadFile($filepath);

        foreach ($file as $row) {
            if(count($row) !== 2) {
                throw new Exception('Invalid CSV file. Must have two columns.');
            }

            // Add the contents to the new file with the new column
            $row[] = basename($filepath);
            $newFile->insertOne($row);
        }

        return $newFile;
    }
}

