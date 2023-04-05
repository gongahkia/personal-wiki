import javax.swing.JFrame;
import javax.swing.JLabel;
import java.awt.Font;

public class NewWindow {

    JFrame frame2 = new JFrame();
    JLabel label2 = new JLabel("alahoo");

    NewWindow() { // constructor 
        label2.setBounds(0,0,100,50);
        label2.setFont(new Font(null, Font.PLAIN, 25));

        frame2.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame2.setSize(430,430);
        frame2.setLayout(null);
        frame2.setVisible(true);

        frame2.add(label2);
    }

}
