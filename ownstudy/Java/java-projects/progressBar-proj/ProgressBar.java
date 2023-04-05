import java.awt.*;
import javax.swing.*;

public class ProgressBar {

    JFrame frame1 = new JFrame();
    JProgressBar progressBar1 = new JProgressBar();

    ProgressBar(){ // constructor
        
        progressBar1.setValue(0);
        progressBar1.setBounds(0,0,420,50);
        progressBar1.setStringPainted(true); // displays the progress of the progress bar with a filled out painted color

        frame1.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame1.setSize(420,420);
        frame1.setLayout(null);
        frame1.setVisible(true);

        frame1.add(progressBar1);

        fill();
    }

    public void fill() {
        int counter = 0;
        while (counter <= 100) {
            progressBar1.setValue(counter);

            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            counter += 10;
        }
        progressBar1.setString("Finished lah you idiot");
    }

}
