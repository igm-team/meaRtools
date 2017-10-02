import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.util.Vector;
/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author dh2744
 */
public class CheckSpkListExpLog {
    public static int checkSpkListExpLog(javax.swing.JTextField spikeListField,
            javax.swing.JTextField expLogFileField,
            javax.swing.JButton runButton,
            javax.swing.JTextArea outputTextArea) {
        
        int exitValue = 0;
        runButton.setEnabled(false);
        Boolean errorLog = false;
        //Vector<String> logResult = new Vector<>();
        // to do java 1.6 compatibility
        Vector<String> logResult = new Vector<String>();
        
        //experimental log
        try {
            //read in Experimental log
           String csvFile = expLogFileField.getText();
           
           String[] csvCheck1 = csvFile.split("\\.");
           String csvText1 = "csv";
           System.out.println(csvCheck1[csvCheck1.length - 1]);
           System.out.println(csvCheck1[csvCheck1.length - 1].equals("csv"));
           if (!(csvCheck1[csvCheck1.length - 1].equals("csv"))) {
               logResult.add( "Exp log file chosen is not a csv file"  );

               ErrorHandler.errorPanel("Exp log file chosen " + '\n'
                       + csvFile + '\n'
                       + "Is not a csv file.");
               exitValue = -1;
               errorLog = true;
           }

           // This will reference one line at a time
           String line = null;
           Boolean expLogGood = true;

            try {
                // FileReader reads text files in the default encoding.
                FileReader fileReader = new FileReader(csvFile);

                // Always wrap FileReader in BufferedReader.
                BufferedReader bufferedReader = new BufferedReader(fileReader);

                line = bufferedReader.readLine();
                String[] expLogParts = line.split(",");
                if (!expLogParts[0].toUpperCase().equals("PROJECT")) {
                    logResult.add( "Error reading in expLog File"  );
                    logResult.add("Exp log file lacks Project column." );
                    ErrorHandler.errorPanel("Exp log file lacks Project column "
                            + '\n'
                            + "Please re-enter exp log file");
                    errorLog = true;
                    expLogGood = false;
                    exitValue = -1;
                } else if (!expLogParts[1].toUpperCase().equals("EXPERIMENT DATE")) {
                        logResult.add( "Error reading in expLog File"  );
                        logResult.add("Exp log file lacks Experiment column." );
                    ErrorHandler.errorPanel("Exp log file lacks Experiment column "
                            + '\n'
                            + "Please re-enter exp log file");
                    errorLog = true;
                    expLogGood = false;
                    exitValue = -1;
                } else {
                    LineNumberReader lnr = null;
                    line = bufferedReader.readLine();
                    Integer i = 1;
                    line = bufferedReader.readLine();
                    String[] country = line.split(",");
                    System.out.println("Start while loop");
                    while (line != null) {
                        System.out.println(line);

                        System.out.println(country[0]);

                        System.out.println("Current line num " + i);
                        i++;
                        line = bufferedReader.readLine();
                        System.out.println(line);
                        System.out.println("line = null? " + (line == null));
                        //System.out.println("line.isEmpty ? " + (line.isEmpty()) );
                        if (line != null) {
                            System.out.println("line : " + line);
                            System.out.println("country : " + country[0]);
                            country = line.split(",");
                        }

                    }
                    System.out.println("Current line num " + i);
                    System.out.println("done with while loop");
                    // Always close files.
                    bufferedReader.close();
                } //end of if-else

            } catch (FileNotFoundException ex) {
                logResult.add( "ExpLog File not found"  );

                System.out.println(
                        "Unable to open file '"
                        + csvFile + "'");
                errorLog = true;
                exitValue = -1;
                expLogGood = false;
            } catch (IOException ex) {
                        logResult.add( "Error reading in expLog File"  );
                        logResult.add("IOException." );
                System.out.println(
                        "Error reading file '"
                        + csvFile + "'");
                errorLog = true;
                expLogGood = false;
                exitValue = -1;

            }
            System.out.println("expLogGood :  " + expLogGood);

            //++++++++++++++++++++++++++++++++++++read in Spk list file  
            String check = spikeListField.getText();
            String[] temp = check.split(",");
            System.out.println("spk list chooser: " + temp[0]);

            String[] spkCsvFile = temp;
            if (temp.length > 1) {
                System.out.println("spkCsvFile.length = " + temp.length);
                for (int ind = 0; ind < temp.length; ind++) {

                    if (0 == ind) {
                        spkCsvFile[ind] = temp[ind].substring(1, temp[ind].length()).trim();
                        System.out.println("spkCsvFile[ind] " + spkCsvFile[ind]);
                        File fileCheck = new File(spkCsvFile[ind]);
                        System.out.println("exists? spkCsvFile[" + ind + "] " + fileCheck.exists());
                    } else if (ind == (temp.length - 1)) {
                        spkCsvFile[ind] = temp[ind].substring(0, temp[ind].length() - 1).trim();
                        System.out.println("spkCsvFile[ind] " + spkCsvFile[ind]);
                        File fileCheck = new File(spkCsvFile[ind]);
                        System.out.println("exists? spkCsvFile[" + ind + "] " + fileCheck.exists());

                    } else {
                        spkCsvFile[ind] = temp[ind];
                        System.out.println("spkCsvFile[ind] " + spkCsvFile[ind].trim());
                        System.out.println("MMMMM" + spkCsvFile[ind].trim() + "MMMMMMM");
                        File fileCheck = new File(spkCsvFile[ind].trim());
                        System.out.println("exists? spkCsvFile[" + ind + "] " + fileCheck.exists());
                    }

                }
            } else if (temp.length == 1) {
                System.out.println("temp.length = " + temp.length);
                System.out.println("temp[0].length = " + temp.length);
                int len = temp[0].length();
                len--;
                spkCsvFile[0] = temp[0].substring(1, len).trim();
                System.out.println("spkCsvFile[ind] " + spkCsvFile[0]);
                File fileCheck = new File(spkCsvFile[0]);
                System.out.println("exists? spkCsvFile[" + 0 + "] " + fileCheck.exists());
            }

            System.out.println("Done with reading in spike-list file names ");

            // check that it's csv
            for (int j = 0; j < spkCsvFile.length; j++) {

                String[] csvCheck = spkCsvFile[j].split("\\.");
                String csvText = "csv";
                System.out.println(csvCheck[csvCheck.length - 1]);
                System.out.println(csvCheck[csvCheck.length - 1].equals("csv"));
                if (!(csvCheck[csvCheck.length - 1].equals("csv"))) {
                        logResult.add( "Spike-list csv file chosen " + spkCsvFile[j]  );
                        logResult.add("Is not a csv file." );

                    ErrorHandler.errorPanel("Spike-list csv file chosen "
                            + spkCsvFile[j] + '\n'
                            + "Is not a csv file.");
                    errorLog = true;
                    exitValue = -1;
                }
                // spike list file
                try {
                    // FileReader reads text files in the default encoding.
                    FileReader fileReader = new FileReader(spkCsvFile[j].trim());

                    System.out.println("file reader " + spkCsvFile[j]);
                    // Always wrap FileReader in BufferedReader.
                    BufferedReader bufferedReader = new BufferedReader(fileReader);

                    line = bufferedReader.readLine();
                    System.out.println(line);
                    String[] spkListHeader = line.split(",");
                    String third = "Time (s)";
                    System.out.println(spkListHeader[2]);
                    // check for spike list file
                    System.out.println(spkListHeader[2]);
                    if (!(spkListHeader[2].equals("Time (s)") || spkListHeader[2].equals("Time (s) ") 
                            || spkListHeader[2].equals("\"Time (s)\"")  
                            || spkListHeader[2].equals("\"Time (s) \""))) {
                        logResult.add( "Spike-list csv file chosen " + spkCsvFile[j]  );
                        logResult.add("Is not a proper spike-list file" );
                        
                        logResult.add( "'Time (s)' should be third column header " );
                        logResult.add("Instead 3rd column is "+ spkListHeader[2].toString() );
                        ErrorHandler.errorPanel("Spike-list csv file chosen '" + spkCsvFile[j]+"'" + '\n'
                                + "Is not a proper spike-list file" + '\n'
                                + "'Time (s)' should be third column header");
                        exitValue = -1;
                        errorLog=true;
                    }

                    for (int counter2 = 1; counter2 < 5; counter2++) {

                        // content check
                        line = bufferedReader.readLine();
                        if (counter2 == 2) {
                            System.out.println(line);

                            String[] spkListContents = line.split(",");

                            //Project,Experiment Date,Plate SN,DIV,Well,Treatment,Size,Dose,Units
                            System.out.println("Time(s) " + spkListContents[2]
                                    + " , Electrode =" + spkListContents[3]);

                        }
                    }

                    // Always close files.
                    bufferedReader.close();
                } catch (FileNotFoundException ex) {
                        logResult.add( " Unable to open file " + spkCsvFile[j]  );
                        logResult.add(" Please select another spike list file " );
                    ErrorHandler.errorPanel("Unable to open file " + j + "       "
                            + spkCsvFile[j] + '\n'
                            + "Please select another spike list file");
                    exitValue = -1;
                    errorLog = true;

                } catch (IOException ex) {
                            logResult.add( "Error reading file "  + spkCsvFile[j] );
                            logResult.add(" IOException " );

                    ErrorHandler.errorPanel(
                            "Error reading file '" + j + "       "
                            + spkCsvFile[j] + "'" + '\n' + "Please select another spike list file");
                    errorLog = true;
                    exitValue = -1;

                }
            } //end of loop through spike List Files

            // +++++++++need to compare files
            System.out.println("Starting match");

            for (int j = 0; j < spkCsvFile.length; j++) {
                if (expLogGood && !errorLog) {
                    try {
                        // FileReader reads text files in the default encoding.
                        FileReader fileReader = new FileReader(csvFile);

                        // Always wrap FileReader in BufferedReader.
                        BufferedReader bufferedReader = new BufferedReader(fileReader);

                        line = bufferedReader.readLine();

                        line = bufferedReader.readLine();

                        System.out.println(line);

                        String[] country = line.split(",");

                        //Project,Experiment Date,Plate SN,DIV,Well,Treatment,Size,Dose,Units
                        if (country.length>5 ){
                            System.out.println("Project " + country[0]
                                + " , Exp Date=" + country[1]
                                + " , Plate SN=" + country[2]
                                + " , DIV= " + country[3]
                                + " , Well=" + country[4]
                                + " , trt=" + country[5]);
                        } else{
                           System.out.println("Project " + country[0]
                                + " , Exp Date=" + country[1]
                                + " , Plate SN=" + country[2]
                                + " , DIV= " + country[3]
                                + " , Well=" + country[4] ); 
                        }


                    // now compare to name of spk-list file
                        File spkListFile = new File(spkCsvFile[j]);
                        System.out.println("spkCsvFile "+spkCsvFile[j]);
                        System.out.println( "File.separator" + File.separator );
                        
                        //String[] spkListFileNameParts = spkCsvFile[j].split(File.separator );
                        //String[] spkListFileNameParts = spkCsvFile[j].split(File.separator );                  
                        //System.out.println(spkListFileNameParts[0]);
                        //System.out.println(spkListFileNameParts[1]);
                        //System.out.println(spkListFileNameParts[2]);
                        //String name1 = spkListFileNameParts[spkListFileNameParts.length - 1];
                        String name1 = spkListFile.getName().toString();
                        String[] partsName1 = name1.split("_");
                        System.out.println(partsName1[0]);
                        System.out.println(partsName1[1]);
                        System.out.println(partsName1[2]);
                        if (!(partsName1[0].equals(country[0].trim() ))) {
                            System.out.println("spkListFileNameParts " + partsName1[0].toString());
                            System.out.println("country " + country[0]);
                            logResult.add( "project in spk-list file name: " + partsName1[0] );
                            logResult.add("& experimental log file project: " + country[0]);
                            logResult.add("Do not match");
                            ErrorHandler.errorPanel(
                                    "project in spk-list file name: " + partsName1[0] + '\n'
                                    + "& experimental log file project: " + country[0] + '\n'
                                    + "Do not match");
                            exitValue = -1;
                            errorLog = true;
                        } else if (!(partsName1[1].equals(country[1].trim() ))) {
                            logResult.add( "Experiment date does not match between" + '\n' + spkCsvFile[j] );
                            logResult.add("and experimental log file. ");
                            logResult.add("Please re-enter file");
                            ErrorHandler.errorPanel(
                                    "Experiment date does not match between" + '\n' + spkCsvFile[j]
                                    + " name and experimental log file. '");
                            exitValue = -1;
                            errorLog = true;
                        } else if (!(partsName1[2].equals(country[2].trim() ))) {
                            logResult.add("No match between spk list" + spkCsvFile[j]);
                            logResult.add("and experimental log file. ");

                            ErrorHandler.errorPanel(
                                    "No match between spk list" + spkCsvFile[j] + '\n'
                                    + "and experimental log file. '"
                                    + '\n' + "Project name do not match");
                            exitValue = -1;
                            errorLog = true;
                        }

                        // Always close files.
                        bufferedReader.close();
                    } catch (FileNotFoundException ex) {
                        System.out.println(
                                "Unable to open file '"
                                + csvFile + "'");
                        logResult.add(" Unable to open file  ");
                        logResult.add("'"  + csvFile + "'");
                        ErrorHandler.errorPanel(
                                    "Unable to open file " + '\n'
                                    + "'"  + csvFile + "'" + '\n'
                                    );
                        exitValue = -1;
                        errorLog = true;

                    } catch (IOException ex) {
                        System.out.println(
                                "Error reading file '"
                                + csvFile + "'");
                        ErrorHandler.errorPanel(
                                    "Error reading file " + 
                                    "'"  + csvFile + "'" + '\n'
                                    );
                        logResult.add("Error reading file ");
                        logResult.add("'"  + csvFile + "'");

                        errorLog = true;
                        exitValue = -1;
                    }
                    System.out.println("expLogGood :  " + expLogGood);
                    System.out.println("          ");
                }
                System.out.println("done with match ");

                System.out.println("NUMER OF TIMES THROUGH LOOP :  " + j + 1);
            } //end of loop through spike list files



            if (!errorLog) {

                runButton.setEnabled(true);
                logResult.add("Files chosen are without errors");
                logResult.add("They match one another");
                logResult.add("proceeeding to Analysis");

            }   

                PrintToTextArea.printToTextArea(logResult , outputTextArea);


        } catch (Exception e) {
            logResult.add("Try at reading the csv file failed");
            logResult.add(" ");
            logResult.add(" ");
            e.printStackTrace();
            exitValue = -1;
            PrintToTextArea.printToTextArea(logResult , outputTextArea);
        }
        return(exitValue);
    }
}
