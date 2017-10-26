
import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;

/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author dh2744
 */
public class SaveParameters {
    public static int saveParameters(
        String rootPath,
        javax.swing.JComboBox getWTComboBox,
        javax.swing.JCheckBox spikeCsvCheckBox,  
        javax.swing.JCheckBox spikePlotCheckBox,
        javax.swing.JSpinner elecMinRateSpinner ,
        javax.swing.JSpinner  elecMaxRateSpinner,
        javax.swing.JSpinner  wellMinRateSpinner,
        javax.swing.JSpinner wellFilterMaxDIVInactiveRatioSpinner,
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
            
        javax.swing.JCheckBox networkBurstCheckBox,
        javax.swing.JSpinner minElectrodesNBSpinner,
        javax.swing.JSpinner firstNBWindowSpinner,
        javax.swing.JSpinner secondNBWindowSpinner,
        javax.swing.JSpinner thirdNBWindowSpinner ){
        int exitValue = 0;
        
        
        try{  // Catch errors in I/O if necessary.
            // Open a file to write to, named SavedObj.sav.
            String fileParams=rootPath.concat("/Parameters.sav");
            System.out.println( fileParams );
            FileOutputStream saveFile=new FileOutputStream( fileParams );

            //try ( ObjectOutputStream save = new ObjectOutputStream(saveFile)) {
            try{
                ObjectOutputStream save = new ObjectOutputStream(saveFile);
                //treatment selection
                save.writeObject( getWTComboBox.getSelectedItem() );
                
                //spikes: output
                save.writeObject( spikeCsvCheckBox.isSelected());       //boolean
                save.writeObject( spikePlotCheckBox.isSelected());//boolean
                
                //spikes: parameter
                save.writeObject( elecMinRateSpinner.getValue());//double
                save.writeObject( elecMaxRateSpinner.getValue() );//double
                save.writeObject( wellMinRateSpinner.getValue() );//Integer
                save.writeObject( wellFilterMaxDIVInactiveRatioSpinner.getValue() );
                save.writeObject( nPermSpinner.getValue() );//Integer
                
                // burst
                //output
                save.writeObject( burstCsvCheckBox.isSelected()); //boolean
                save.writeObject( burstPlotCheckBox.isSelected()); //boolean
                
                
                // algorithm
                // poisson surprise
                save.writeObject( poissonSurpriseRadioButton.isSelected() );//boolean
                save.writeObject( surpriseLevelSpinner.getValue() );//integer
                save.writeObject( maxIntMethodRadioButton.isSelected() );//boolean
                
                
                //beg.isi
                save.writeObject( begISISpinner.getValue());//double
                save.writeObject( minIBISpinner.getValue());//double
                save.writeObject( minDurSpinner.getValue() ); //double
                save.writeObject(  endISISpinner.getValue() );//double
                save.writeObject( minSpkSpinner.getValue());//Integer
                
                // NS
                //output
                save.writeObject( nsCsvCheckBox.isSelected());//boolean
                save.writeObject( nsPlotCheckBox.isSelected());//boolean
                
                //parameters
                save.writeObject( nsTSpinner.getValue() );//doule              
                save.writeObject( nsNSpinner.getValue() );//Integer                
                save.writeObject( surSpinner.getValue());//Integer
                
                //distribution
                save.writeObject( minIBIJSpinner.getValue());//Integer
                save.writeObject( minISIJSpinner.getValue());//Integer
                save.writeObject( minDurationJSpinner.getValue());//Integer
                save.writeObject( minNSpikesJSpinner.getValue());//Integer
                save.writeObject( minSpikeFreqJSpinner.getValue());//Integer
                
                save.writeObject( xlimIBISpinner.getValue());//Integer
                save.writeObject( xlimISISpinner.getValue());//Integer
                save.writeObject( xlimDurationSpinner.getValue());//Integer
                save.writeObject( xlimNSpikesSpinner.getValue());//Integer
                save.writeObject( xlimSpikeFreqSpinner.getValue());//Integer
                
                save.writeObject( binsInSegIBISpinner.getValue());//Integer
                save.writeObject( binsInSegISISpinner.getValue());//Integer
                save.writeObject( binsInSegDurationSpinner.getValue());//Integer
                save.writeObject( binsInSegNSpikesSpinner.getValue());//Integer
                save.writeObject( binsInSegSpikeFreqSpinner.getValue());//Integer
                
                
                save.writeObject( IBIPerWellCheckBox.isSelected());
                save.writeObject( ISIPerWellCheckBox.isSelected());
                save.writeObject( durationPerWellCheckBox.isSelected());
                save.writeObject( nSpikesPerWellCheckBox.isSelected());
                save.writeObject( spikeFreqPerWellCheckBox.isSelected());
                
                save.writeObject( performIBICheckBox.isSelected());
                save.writeObject( performISICheckBox.isSelected());
                save.writeObject( performDurationCheckBox.isSelected());
                save.writeObject( performNSpikesCheckBox.isSelected());
                save.writeObject( performSpikeFreqCheckbox.isSelected());
                
                //network events
                save.writeObject( networkBurstCheckBox.isSelected());
                save.writeObject( minElectrodesNBSpinner.getValue() );
                save.writeObject( firstNBWindowSpinner.getValue() ) ;
                save.writeObject( secondNBWindowSpinner.getValue() ) ;
                save.writeObject( thirdNBWindowSpinner.getValue() ) ;
                

                // Close the file.
                save.close(); // This also closes saveFile.
            //new to be compatible 1.6
            } catch(  Exception exc ){
                
                
            }
            

        } catch(Exception exc){
            exitValue=-1;
        }

        
        
        return(exitValue);
    } 
}
