/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author dh2744
 */
public class ConvertParameters {
    public static int convertParameters(
        javax.swing.JCheckBox spikeCsvCheckBox,  
        javax.swing.JCheckBox spikePlotCheckBox,
        javax.swing.JSpinner elecMinRateSpinner ,
        javax.swing.JSpinner  elecMaxRateSpinner,
        javax.swing.JSpinner  wellMinRateSpinner,
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
        javax.swing.JSpinner minValuesIBISpinner,
        javax.swing.JSpinner minValuesISISpinner,
        javax.swing.JSpinner minValuesDurationSpinner,
        javax.swing.JSpinner minValuesNSpikesSpinner,
        javax.swing.JSpinner minValuesSpikeFreqSpinner,

        javax.swing.JCheckBox filterByWellIBICheckBox,
        javax.swing.JCheckBox filterByWellISICheckBox,
        javax.swing.JCheckBox filterByWellDurationCheckBox,
        javax.swing.JCheckBox filterByWellnSpikesCheckBox,
        javax.swing.JCheckBox filterByWellSpikeFreqCheckBox,

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
            
        javax.swing.JCheckBox networkBurstCheckBox,
        javax.swing.JSpinner localRegionMinAESpinner,
        javax.swing.JSpinner minElectrodesNBSpinner,
        javax.swing.JSpinner firstNBWindowSpinner,
        javax.swing.JSpinner secondNBWindowSpinner,
        javax.swing.JSpinner thirdNBWindowSpinner ){
        
        int exitValue = 0;
                
        try{  // Catch errors in I/O if necessary.
            // spikes
            //output
            Boolean wantSpikesCSV = new Boolean(spikeCsvCheckBox.isSelected());
            String wantSpikeCSVstr = String.valueOf(wantSpikesCSV);
            Boolean wantSpikePlot = new Boolean(spikePlotCheckBox.isSelected());
            String wantSpikePlotstr = String.valueOf(wantSpikePlot);

            //spikes: parameter
            Double elecMinRateValue = (Double) elecMinRateSpinner.getValue();
            String elecMinRateChar = String.valueOf(elecMinRateValue);

            Double elecMaxRateValue = (Double) elecMaxRateSpinner.getValue();
            String elecMaxRateChar = String.valueOf(elecMaxRateValue);

            Integer wellMinRateValue = (Integer) wellMinRateSpinner.getValue();
            String wellMinRateChar = String.valueOf(wellMinRateValue);

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
            Double endIsiValue = (Double) minIBISpinner.getValue();
            String endIsiChar = String.valueOf(endIsiValue);

            //min.ibi
            Double minIbiValue = (Double) minDurSpinner.getValue();
            String minIbiChar = String.valueOf(minIbiValue);

            //min.durn
            Double minDurValue = (Double) endISISpinner.getValue();
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

            String minIBIChar = String.valueOf(minIBIValue);
            String minISIChar = String.valueOf(minISIValue);
            String minDurationChar = String.valueOf(minDurationValue);
            String minNSpikesChar = String.valueOf(minNSpikesValue);
            String minSpikeFreqChar = String.valueOf(minSpikeFreqValue);

            Integer xlimIBIValue = (Integer) xlimIBISpinner.getValue();
            Integer xlimISIValue = (Integer) xlimISISpinner.getValue();
            Integer xlimDurationValue = (Integer) xlimDurationSpinner.getValue();
            Integer xlimNSpikesValue = (Integer) xlimNSpikesSpinner.getValue();
            Integer xlimSpikeFreqValue = (Integer) xlimSpikeFreqSpinner.getValue();

            String xlimIBIChar = String.valueOf(xlimIBIValue);
            String xlimISIChar = String.valueOf(xlimISIValue);
            String xlimDurationChar = String.valueOf(xlimDurationValue);
            String xlimNSpikesChar = String.valueOf(xlimNSpikesValue);
            String xlimSpikeFreqChar = String.valueOf(xlimSpikeFreqValue);

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

            Integer minValuesIBIValue = (Integer) minValuesIBISpinner.getValue();
            Integer minValuesISIValue = (Integer) minValuesISISpinner.getValue();
            Integer minValuesDurationValue = (Integer) minValuesDurationSpinner.getValue();
            Integer minValuesNSpikesValue = (Integer) minValuesNSpikesSpinner.getValue();
            Integer minValuesSpikeFreqValue = (Integer) minValuesSpikeFreqSpinner.getValue();

            String minValuesIBIChar = String.valueOf(minValuesIBIValue);
            String minValuesISIChar = String.valueOf(minValuesISIValue);
            String minValuesDurationChar = String.valueOf(minValuesDurationValue);
            String minValuesNSpikesChar = String.valueOf(minValuesNSpikesValue);
            String minValuesSpikeFreqChar = String.valueOf(minValuesSpikeFreqValue);

            Boolean filterByWellIBIBool = new Boolean(filterByWellIBICheckBox.isSelected());
            Boolean filterByWellISIBool = new Boolean(filterByWellISICheckBox.isSelected());
            Boolean filterByWellDurationBool = new Boolean(filterByWellDurationCheckBox.isSelected());
            Boolean filterByWellnSpikesBool = new Boolean(filterByWellnSpikesCheckBox.isSelected());
            Boolean filterByWellSpikeFreqBool = new Boolean(filterByWellSpikeFreqCheckBox.isSelected());

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
            Boolean NBChecked = networkBurstCheckBox.isSelected() ;
            Integer minAENB = (Integer) localRegionMinAESpinner.getValue() ;
            Integer minElectrodesNB = (Integer) minElectrodesNBSpinner.getValue() ;
            Integer firstNBWindow= (Integer) firstNBWindowSpinner.getValue();
            Integer secondNBWindow= (Integer) secondNBWindowSpinner.getValue();
            Integer thirdNBWindow= (Integer) thirdNBWindowSpinner.getValue();

            String NBCheckedChar = String.valueOf( NBChecked) ;
            String minAENBChar = String.valueOf( minAENB ) ;
            String minElectrodesNBChar = String.valueOf( minElectrodesNB ) ;
            String firstNBWindowChar = String.valueOf( firstNBWindow ) ;
            String secondNBWindowChar = String.valueOf( secondNBWindow ) ;
            String thirdNBWindowChar = String.valueOf( thirdNBWindow ) ;
            
        } catch(Exception e){
            exitValue = 1;
        }
        return(exitValue);
    }
    
}
