
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author dh2744
 */
public class WriteParameterR {
    public static int writeParameterR(
        String rootPath,
        String[] spkCsvFile,
        String expLogFile,
        javax.swing.JComboBox getWTComboBox,
        javax.swing.JCheckBox spikeCsvCheckBox,  
        javax.swing.JCheckBox spikePlotCheckBox,
        javax.swing.JSpinner elecMinRateSpinner ,
        javax.swing.JSpinner  elecMaxRateSpinner,
        javax.swing.JSpinner  wellMinRateSpinner,
        javax.swing.JSpinner  wellFilterMaxDIVInactiveRatioSpinner,
        
        javax.swing.JSpinner  nPermSpinner,
        javax.swing.JCheckBox burstCsvCheckBox,
        javax.swing.JCheckBox burstPlotCheckBox,
        javax.swing.JRadioButton poissonSurpriseRadioButton,
        javax.swing.JSpinner  surpriseLevelSpinner,
        javax.swing.JRadioButton maxIntMethodRadioButton,
        javax.swing.JSpinner  begISISpinner,
        javax.swing.JSpinner minIBISpinner,
        javax.swing.JSpinner minDurSpinner,
        javax.swing.JSpinner endISISpinner,
        javax.swing.JSpinner  minSpkSpinner,
        javax.swing.JCheckBox  nsCsvCheckBox,
        javax.swing.JCheckBox  nsPlotCheckBox,
        javax.swing.JSpinner nsTSpinner,
        javax.swing.JSpinner nsNSpinner,
        javax.swing.JSpinner surSpinner,
        javax.swing.JSpinner minIBIJSpinner,
        javax.swing.JSpinner minISIJSpinner ,
        javax.swing.JSpinner minDurationJSpinner,
        javax.swing.JSpinner minNSpikesJSpinner,
        javax.swing.JSpinner minSpikeFreqJSpinner,
        javax.swing.JSpinner xlimIBISpinner,
        javax.swing.JSpinner xlimISISpinner,
        javax.swing.JSpinner  xlimDurationSpinner,
        javax.swing.JSpinner  xlimNSpikesSpinner,
        javax.swing.JSpinner  xlimSpikeFreqSpinner,
        javax.swing.JSpinner  binsInSegIBISpinner,
        javax.swing.JSpinner binsInSegISISpinner,
        javax.swing.JSpinner binsInSegDurationSpinner,
        javax.swing.JSpinner binsInSegNSpikesSpinner,
        javax.swing.JSpinner binsInSegSpikeFreqSpinner,

        javax.swing.JCheckBox IBIPerWellCheckBox,
        javax.swing.JCheckBox ISIPerWellCheckBox,
        javax.swing.JCheckBox  durationPerWellCheckBox,
        javax.swing.JCheckBox nSpikesPerWellCheckBox,
        javax.swing.JCheckBox spikeFreqPerWellCheckBox,

        javax.swing.JCheckBox  performIBICheckBox,
        javax.swing.JCheckBox performISICheckBox,
        javax.swing.JCheckBox performDurationCheckBox,
        javax.swing.JCheckBox performNSpikesCheckBox,
        javax.swing.JCheckBox performSpikeFreqCheckbox,
        javax.swing.JCheckBox  networkBurstCheckBox, 
        javax.swing.JSpinner minElectrodesNBSpinner,
        javax.swing.JSpinner firstNBWindowSpinner,
        javax.swing.JSpinner secondNBWindowSpinner,
        javax.swing.JSpinner thirdNBWindowSpinner 
      
      ){
        int exitValue = 0;
        try {
            FileWriter f = null;
            try {
                

                System.out.println( "parameter file:  " + rootPath + 
                        File.separator+"Code"+File.separator+ "parameters.R" );

             f = new FileWriter(rootPath + File.separator+"Code"+File.separator+ "parameters.R");
             //f = new FileWriter("/Users/dh2744/Dropbox/Columbia/Software/github/mea/MEA_analysis/Code/parameters.R");
            } catch (IOException ex) {
                System.out.println("error in opening parameter file");
                Logger.getLogger(IGM_MEA_main.class.getName()).log(Level.SEVERE, null, ex);
            }
            
            //convert objects into values
            String WTSelected = String.valueOf(getWTComboBox.getSelectedItem());
            // spikes
            //output
            Boolean wantSpikesCSV = new Boolean(spikeCsvCheckBox.isSelected());
            String wantSpikeCSVstr = String.valueOf(wantSpikesCSV);
            Boolean wantSpikePlot = new Boolean(spikePlotCheckBox.isSelected());
            String wantSpikePlotstr = String.valueOf(wantSpikePlot);

            //spikes: parameter
            Double elecMinRateValue = (Double) elecMinRateSpinner.getValue();
            String elecMinRateChar = String.valueOf(elecMinRateValue);

            Integer elecMaxRateValue = (Integer) elecMaxRateSpinner.getValue();
            String elecMaxRateChar = String.valueOf(elecMaxRateValue);

            Integer wellMinRateValue = (Integer) wellMinRateSpinner.getValue();
            String wellMinRateChar = String.valueOf(wellMinRateValue);
            
            Double wellFilterMaxDIVInactiveRatioValue= (Double) wellFilterMaxDIVInactiveRatioSpinner.getValue();
            String wellFilterMaxDIVInactiveRatioChar= String.valueOf(wellFilterMaxDIVInactiveRatioValue);
            
            Integer nPermValue=(Integer )  nPermSpinner.getValue();
            String  nPermChar = String.valueOf(nPermValue);

            // burst
            //output
            Boolean wantburstCSV = new Boolean(burstCsvCheckBox.isSelected());
            String wantburstCSVstr = String.valueOf(wantburstCSV);
            Boolean wantburstPlot = new Boolean(burstPlotCheckBox.isSelected());
            String wantburstPlotstr = String.valueOf(wantburstPlot);

            // algorithm
            // poisson surprise
            Boolean psButtonBool = poissonSurpriseRadioButton.isSelected();
            String psButtonStr = String.valueOf(psButtonBool);

            Integer surpriseLevelValue = (Integer) surpriseLevelSpinner.getValue();
            String surpriseLevelChar = String.valueOf(surpriseLevelValue);


            //maxIntMethodButton
            Boolean maxIntMethodValue = maxIntMethodRadioButton.isSelected();
            String maxIntMethodStr = String.valueOf(maxIntMethodValue);

            //beg.isi
            Double begIsiValue = (Double) begISISpinner.getValue();
            String begIsiChar = String.valueOf(begIsiValue);

            //end.isi
            Double endIsiValue = (Double) endISISpinner.getValue();
            String endIsiChar = String.valueOf(endIsiValue);

            //min.ibi
            Double minIbiValue = (Double) minIBISpinner.getValue();
            String minIbiChar = String.valueOf(minIbiValue);

            //min.durn
            Double minDurValue = (Double) minDurSpinner.getValue();
            String minDurChar = String.valueOf(minDurValue);

            //min.spikes
            Integer minSpkValue = (Integer) minSpkSpinner.getValue();
            String minSpkChar = String.valueOf(minSpkValue);

            // NS
            //output
            Boolean wantnsCSV = new Boolean(nsCsvCheckBox.isSelected());
            String wantnsCSVstr = String.valueOf(wantnsCSV);
            Boolean wantnsPlot = new Boolean(nsPlotCheckBox.isSelected());
            String wantnsPlotstr = String.valueOf(wantnsPlot);

            //parameters
            Double nsTValue = (Double) nsTSpinner.getValue();
            String nsTChar = String.valueOf(nsTValue);

            Integer nsNValue = (Integer) nsNSpinner.getValue();
            String nsNChar = String.valueOf(nsNValue);

            Integer surValue = (Integer) surSpinner.getValue();
            String surChar = String.valueOf(surValue);
            
            

            //distribution
            Integer minIBIValue = (Integer) minIBIJSpinner.getValue();
            Integer minISIValue = (Integer) minISIJSpinner.getValue();
            Integer minDurationValue = (Integer) minDurationJSpinner.getValue();
            Integer minNSpikesValue = (Integer) minNSpikesJSpinner.getValue();
            Integer minSpikeFreqValue = (Integer) minSpikeFreqJSpinner.getValue();

            String minIBIDistChar = String.valueOf(minIBIValue);
            String minISIDistChar = String.valueOf(minISIValue);
            String minDurationDistChar = String.valueOf(minDurationValue);
            String minNSpikesDistChar = String.valueOf(minNSpikesValue);
            String minSpikeFreqChar = String.valueOf(minSpikeFreqValue);

            Integer xlimIBIValue = (Integer) xlimIBISpinner.getValue();
            Double  xlimISIValue = (Double) xlimISISpinner.getValue();
            Integer xlimDurationValue = (Integer) xlimDurationSpinner.getValue();
            Integer xlimNSpikesValue = (Integer) xlimNSpikesSpinner.getValue();
            Integer xlimSpikeFreqValue = (Integer) xlimSpikeFreqSpinner.getValue();

            String xlimIBIChar = String.valueOf(xlimIBIValue);
            String xlimISIChar = String.valueOf(xlimISIValue);
            String xlimDurationChar = String.valueOf(xlimDurationValue);
            String xlimNSpikesChar = String.valueOf(xlimNSpikesValue);
            String xlimSpikeFreqChar = String.valueOf(xlimSpikeFreqValue);
            
            System.out.println("xlimIBIChar " + xlimIBIChar );
            System.out.println("xlimISIChar " + xlimISIChar );
            System.out.println("xlimDurationChar " + xlimDurationChar );
            System.out.println("xlimNSpikesChar " + xlimNSpikesChar );
            System.out.println("xlimSpikeFreqChar " + xlimSpikeFreqChar );

            Integer binsInSegIBIValue = (Integer) binsInSegIBISpinner.getValue();
            Integer binsInSegISIValue = (Integer) binsInSegISISpinner.getValue();
            Integer binsInSegDurationValue = (Integer) binsInSegDurationSpinner.getValue();
            Integer binsInSegNSpikesValue = (Integer) binsInSegNSpikesSpinner.getValue();
            Integer binsInSegSpikeFreqValue = (Integer) binsInSegSpikeFreqSpinner.getValue();
            

            String binsInSegIBIChar = String.valueOf(binsInSegIBIValue);
            String binsInSegISIChar = String.valueOf(binsInSegISIValue);
            String binsInSegDurationChar = String.valueOf(binsInSegDurationValue);
            String binsInSegNSpikesChar = String.valueOf(binsInSegNSpikesValue);
            String binsInSegSpikeFreqChar = String.valueOf(binsInSegSpikeFreqValue);
            
            System.out.println("binsInSegIBIChar " + binsInSegIBIChar );
            System.out.println("binsInSegISIChar " + binsInSegISIChar );
            System.out.println("binsInSegDurationChar " + binsInSegDurationChar );
            System.out.println("binsInSegNSpikesChar " + binsInSegNSpikesChar );
            System.out.println("binsInSegSpikeFreqChar " + binsInSegSpikeFreqChar );

            Integer minValuesIBIValue = (Integer) 0;
            Integer minValuesISIValue = (Integer) 0;
            Integer minValuesDurationValue = (Integer) 0;
            Integer minValuesNSpikesValue = (Integer) 0;
            Integer minValuesSpikeFreqValue = (Integer) 0;

            String minValuesIBIChar = String.valueOf(minValuesIBIValue);
            String minValuesISIChar = String.valueOf(minValuesISIValue);
            String minValuesDurationChar = String.valueOf(minValuesDurationValue);
            String minValuesNSpikesChar = String.valueOf(minValuesNSpikesValue);
            String minValuesSpikeFreqChar = String.valueOf(minValuesSpikeFreqValue);

            Boolean filterByWellIBIBool = new Boolean(0>1);
            Boolean filterByWellISIBool = new Boolean(0>1);
            Boolean filterByWellDurationBool = new Boolean(0>1);
            Boolean filterByWellnSpikesBool = new Boolean(0>1);
            Boolean filterByWellSpikeFreqBool = new Boolean(0>1);

            String filterByWellIBIChar = String.valueOf(filterByWellIBIBool);
            String filterByWellISIChar = String.valueOf(filterByWellISIBool);
            String filterByWellDurationChar = String.valueOf(filterByWellDurationBool);
            String filterByWellnSpikesChar = String.valueOf(filterByWellnSpikesBool);
            String filterByWellSpikeFreqChar = String.valueOf(filterByWellSpikeFreqBool);

            Boolean IBIPerWellBool = new Boolean(IBIPerWellCheckBox.isSelected());
            Boolean ISIPerWellBool = new Boolean(ISIPerWellCheckBox.isSelected());
            Boolean durationPerWellBool = new Boolean(durationPerWellCheckBox.isSelected());
            Boolean nSpikesPerWellBool = new Boolean(nSpikesPerWellCheckBox.isSelected());
            Boolean spikeFreqPerWellBool = new Boolean(spikeFreqPerWellCheckBox.isSelected());

            String IBIPerWellChar = String.valueOf(IBIPerWellBool);
            String ISIPerWellChar = String.valueOf(ISIPerWellBool);
            String durationPerWellChar = String.valueOf(durationPerWellBool);
            String nSpikesPerWellChar = String.valueOf(nSpikesPerWellBool);
            String spikeFreqPerWellChar = String.valueOf(spikeFreqPerWellBool);

            Boolean performIBIBool = new Boolean(performIBICheckBox.isSelected());
            Boolean performISIBool = new Boolean(performISICheckBox.isSelected());
            Boolean performDurationBool = new Boolean(performDurationCheckBox.isSelected());
            Boolean performNSpikesBool = new Boolean(performNSpikesCheckBox.isSelected());
            Boolean performSpikeFreqBool = new Boolean(performSpikeFreqCheckbox.isSelected());

            String performIBIChar = String.valueOf(performIBIBool);
            String performISIChar = String.valueOf(performISIBool);
            String performDurationChar = String.valueOf(performDurationBool);
            String performNSpikesChar = String.valueOf(performNSpikesBool);
            String performSpikeFreqChar = String.valueOf(performSpikeFreqBool);

            //network burst
            Boolean nbBool =networkBurstCheckBox.isSelected();
            Integer minElectrodesNB = (Integer) minElectrodesNBSpinner.getValue() ;
            Integer firstNBWindow= (Integer) firstNBWindowSpinner.getValue();
            Integer secondNBWindow= (Integer) secondNBWindowSpinner.getValue();
            Integer thirdNBWindow= (Integer) thirdNBWindowSpinner.getValue();

            String nbChar = String.valueOf( nbBool );
            String minElectrodesNBChar = String.valueOf( minElectrodesNB ) ;
            String firstNBWindowChar = String.valueOf( firstNBWindow ) ;
            String secondNBWindowChar = String.valueOf( secondNBWindow ) ;
            String thirdNBWindowChar = String.valueOf( thirdNBWindow ) ;


            
            
            
            
            DateFormat df = new SimpleDateFormat("yyyy-MM-dd-hh_mm_ss");
            System.out.println(" df "+df);
            Date dateobj = new Date();
            System.out.println(" dateobj "+ dateobj );
            String dateString1=dateobj.toString();
            String dateString= df.format(dateobj);
            System.out.println("dateString "+ dateString );

            // files
            f.write("# executed : " + df.format(dateobj) );
            f.write("\n");
            f.write("analysis<-list();");
            f.write("\n");
            f.write("spk_list_files<-c();");
            f.write("\n");
            for (int i = 0; i < spkCsvFile.length; i++) {
                int result = i + 1;
                f.write("analysis$spk_list_files[" + result + "]<-'"
                        + spkCsvFile[i].trim() + "';");
                f.write("\n");
                f.write("spk_list_files[" + result + "]<-'"
                        + spkCsvFile[i].trim() + "';" );
                f.write("\n");
            }
            f.write("\n");
            f.write("experimental_log_file<-'" + expLogFile.trim() + "';");
            f.write("\n");
            f.write("\n");
            String sep = File.separator;
            
            f.write("analysis$Routput_dir<-'Analysis"+File.separator+File.separator+ "R_objects' ");
            f.write("\n");
            f.write("analysis$output_dir<-'Analysis'");
            f.write("\n");
            f.write("\n");

            f.write("parameters<- list( ");
            f.write("\n");
            f.write("wt=\"");
            f.write(WTSelected);
            f.write("\",");
            f.write("\n");
            // ++++++++++++++++++++GUI switches 
            //spikes
            f.write(" # spikes");
            f.write("\n");
            f.write("spike_csv=");
            f.write(wantSpikeCSVstr.toUpperCase());
            f.write(",");

            f.write("\n");
            f.write("spike_plot=");
            f.write(wantSpikePlotstr.toUpperCase());
            f.write(",");
            f.write("\n");
            //bursts
            f.write("\n");
            f.write("\n");
            f.write("# Burst ");
            f.write("\n");
            f.write("burst_csv=");
            f.write(wantburstCSVstr.toUpperCase());
            f.write(",");

            f.write("\n");
            f.write("burst_plot=");
            f.write(wantburstPlotstr.toUpperCase());
            f.write(",");
            f.write("\n");
            //network spikes
            f.write("\n");
            f.write("\n");
            f.write("# network spikes ");
            f.write("\n");
            f.write("ns_csv=");
            f.write(wantnsCSVstr.toUpperCase());
            f.write(",");

            f.write("\n");
            f.write("ns_plot=");
            f.write(wantnsPlotstr.toUpperCase());
            f.write(",");
            f.write("\n");
            
            // matches IGM.MEA/data/parameters.R
            // BURST
            f.write("\n");
            if (psButtonBool) {
                f.write("burst_type=\"ps\" ");
            } else {
                f.write("burst_type=\"mi\" ");
            }
            f.write(",");

            f.write("\n");
            f.write("s_min=" + surpriseLevelChar );
            f.write("\n");
            f.write(",");
            
            //hard code this since it's not clear to me what this # perm does,
            //for Ryan p-value I suspect
            f.write("\n");
            f.write("perm_n =" + nPermChar );
            f.write(",");
            f.write("\n");
           

            f.write("\n");
            f.write("elec_min_rate=");
            f.write(elecMinRateChar);
            f.write(",");

            f.write("\n");
            f.write("elec_max_rate=");
            f.write(elecMaxRateChar);
            f.write(",");

            f.write("\n");
            f.write("well_min_rate=");
            f.write(wellMinRateChar);
            f.write(",");
            
            f.write("\n");
            f.write("well_filter_maximum_div_inactive_ratio=");
            f.write(wellFilterMaxDIVInactiveRatioChar);
            f.write(",");

            
            
            f.write("\n");
            f.write("\n");
            f.write("mi_par=list(beg_isi =");
            f.write(begIsiChar);
            f.write(",");

            f.write("\n");
            f.write("end_isi =");
            f.write(endIsiChar);
            f.write(",");

            f.write("\n");
            f.write("min_ibi =");
            f.write(minIbiChar);
            f.write(",");

            f.write("\n");
            f.write("min_durn =");
            f.write(minDurChar);
            f.write(",");

            f.write("\n");
            f.write("min_spikes = ");
            f.write(minSpkChar);
            f.write("),");
            f.write("\n");
            f.write("\n");

            // network spikes
            
            f.write("\n");
            f.write("ns_t=");
            f.write(nsTChar);
            f.write(",");

            f.write("\n");
            f.write("ns_n=");
            f.write(nsNChar);
            f.write(",");

            f.write("\n");
            f.write("sur=");
            f.write(surChar);
            f.write(",");
            f.write("\n");

            // distribution
            f.write("\n");
            f.write("# Distributions IBI");
            f.write("\n");
            f.write("burst_distribution_ibi=list(");

            f.write("\n");
            f.write("perform = ");
            if (performIBIBool) {
                f.write("1");
            } else {
                f.write("0");
            }
            f.write(",");

            f.write("\n");
            f.write("min_cases = ");
            f.write(minIBIDistChar);
            f.write(",");

            f.write("\n");
            f.write("x_axis_lim = ");
            f.write(xlimIBIChar);
            f.write(",");

            f.write("\n");
            f.write("bins_in_sec = ");
            f.write(binsInSegIBIChar);
            f.write(",");

            f.write("\n");
            f.write("min_values = ");
            f.write(minValuesIBIChar);
            f.write(",");

            f.write("\n");
            f.write("filter_by_min = ");
            if (filterByWellIBIBool) {
                f.write("1");
            } else {
                f.write("0");
            }
            f.write(",");

            f.write("\n");
            f.write("per_well = ");
            if (IBIPerWellBool) {
                f.write("1");
            } else {
                f.write("0");
            }
            f.write("), ");
            f.write("\n");
            f.write("\n");

            //burst duration
            f.write("# burst duration distribution parameters");

            f.write("\n");
            f.write("burst_distribution_durn=list(");

            f.write("\n");
            f.write("perform = ");
            if (performDurationBool) {
                f.write("1");
            } else {
                f.write("0");
            }
            f.write(",");

            f.write("\n");
            f.write("min_cases = ");
            f.write(minDurationDistChar);
            f.write(",");

            f.write("\n");
            f.write("x_axis_lim = ");
            f.write(xlimDurationChar);
            f.write(",");

            f.write("\n");
            f.write("bins_in_sec = ");
            f.write(binsInSegIBIChar);
            f.write(",");

            f.write("\n");
            f.write("min_values = ");
            f.write(minValuesDurationChar);
            f.write(",");

            f.write("\n");
            f.write("filter_by_min = ");
            if (filterByWellDurationBool) {
                f.write("1");
            } else {
                f.write("0");
            }
            f.write(",");

            f.write("\n");
            f.write("per_well = ");
            if (durationPerWellBool) {
                f.write("1");
            } else {
                f.write("0");
            }
            f.write("), ");
            f.write("\n");
            f.write("\n");

            //burst distribution ISI
            f.write("# burst distribution ISI parameters");

            f.write("\n");
            f.write("burst_distribution_isi=list(");

            f.write("\n");
            f.write("perform = ");
            if (performISIBool) {
                f.write("1");
            } else {
                f.write("0");
            }
            f.write(",");

            f.write("\n");
            f.write("min_cases = ");
            f.write(minISIDistChar);
            f.write(",");

            f.write("\n");
            f.write("x_axis_lim = ");
            f.write(xlimISIChar);
            f.write(",");

            f.write("\n");
            f.write("bins_in_sec = ");
            f.write(binsInSegISIChar);
            f.write(",");

            f.write("\n");
            f.write("min_values = ");
            f.write(minValuesDurationChar);
            f.write(",");

            f.write("\n");
            f.write("filter_by_min = ");
            if (filterByWellISIBool) {
                f.write("1");
            } else {
                f.write("0");
            }
            f.write(",");

            f.write("\n");
            f.write("per_well = ");
            if (ISIPerWellBool) {
                f.write("1");
            } else {
                f.write("0");
            }
            f.write("), ");
            f.write("\n");
            f.write("\n");

            //burst nSpikes
            f.write("# burst distribution nspikes parameters");

            f.write("\n");
            f.write("burst_distribution_nspikes=list(");

            f.write("\n");
            f.write("perform = ");
            if (performNSpikesBool) {
                f.write("1");
            } else {
                f.write("0");
            }
            f.write(",");

            f.write("\n");
            f.write("min_cases = ");
            f.write(minNSpikesDistChar);
            f.write(",");

            f.write("\n");
            f.write("x_axis_lim = ");
            f.write(xlimNSpikesChar);
            f.write(",");

            f.write("\n");
            f.write("bins_in_sec = ");
            f.write(binsInSegNSpikesChar);
            f.write(",");

            f.write("\n");
            f.write("min_values = ");
            f.write(minValuesDurationChar);
            f.write(",");

            f.write("\n");
            f.write("filter_by_min = ");
            if (filterByWellnSpikesBool) {
                f.write("1");
            } else {
                f.write("0");
            }
            f.write(",");

            f.write("\n");
            f.write("per_well = ");
            if (nSpikesPerWellBool) {
                f.write("1");
            } else {
                f.write("0");
            }
            f.write(") ");
            f.write(",");
            f.write("\n");
            f.write("\n");

            //burst nSPikes
            f.write("# burst distribution spikeFreq parameters");

            f.write("\n");
            f.write("burst_distribution_spike_freq=list(");

            f.write("\n");
            f.write("perform = ");
            if (performSpikeFreqBool) {
                f.write("1");
            } else {
                f.write("0");
            }
            f.write(",");

            f.write("\n");
            f.write("min_cases = ");
            f.write(minSpikeFreqChar);
            f.write(",");

            f.write("\n");
            f.write("x_axis_lim = ");
            f.write(xlimSpikeFreqChar);
            f.write(",");

            f.write("\n");
            f.write("bins_in_sec = ");
            f.write(binsInSegSpikeFreqChar);
            f.write(",");

            f.write("\n");
            f.write("min_values = ");
            f.write(minValuesSpikeFreqChar);
            f.write(",");

            f.write("\n");
            f.write("filter_by_min = ");
            if (filterByWellSpikeFreqBool) {
                f.write("1");
            } else {
                f.write("0");
            }
            f.write(",");

            f.write("\n");
            f.write("per_well = ");
            if (spikeFreqPerWellBool) {
                f.write("1");
            } else {
                f.write("0");
            }
            f.write("), ");
            f.write("\n");
            f.write("\n");
            
            //NB
            // must write this even hard coded, user doesn't get to change
            f.write("\n");
            f.write("want_nb=");
            f.write(nbChar.toUpperCase());
            f.write(",");
            f.write("\n");
            
            f.write("\n");
            f.write("local_region_min_nae= 0,");
            f.write("\n");
            f.write("min_electrodes = ");
            f.write(minElectrodesNBChar);
            f.write(",");
            
            
            f.write("\n");
            f.write("sigma = c(");
            f.write(firstNBWindowChar);
            f.write(",");
            f.write(secondNBWindowChar);
            f.write(",");
            f.write(thirdNBWindowChar);
            f.write(")");
            f.write("\n");
            f.write("\n");
            //add date time
            f.write(",");
            f.write("timeStamp='" + df.format(dateobj)+ "'");
            f.write("\n");
            
            f.write(")"); //end the parameters<-list( 
            f.write("\n");
            f.write("\n");
            //String NBCheckedChar = String.valueOf( NBChecked) ;
        //String minAENBChar = String.valueOf( minAENB ) ;
        //String minElectrodesNBChar = String.valueOf( minElectrodesNB ) ;
  //      String sigmaChar= String.valueOf( sigma ) ;

            f.close();
            
            System.out.println(" After write Parameters.R " );
            

        } catch (Exception ex) {
            System.out.println("error in try catch before writing to log file");
            Logger.getLogger(IGM_MEA_main.class.getName()).log(Level.SEVERE, null, ex);
            exitValue = -1;
        }


        return(exitValue);
    }
}
