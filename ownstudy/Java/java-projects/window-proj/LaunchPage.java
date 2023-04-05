import javax.swing.JFrame;
import javax.swing.JButton;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

public class LaunchPage implements ActionListener{
    
    JFrame frame1 = new JFrame();
    JButton button1 = new JButton();

    LaunchPage() { // constructor
        button1.setBounds(100,160,200,40);
        button1.setFocusable(false);
        button1.addActionListener(this); // note that we refer to the parameters of the .addActionListener method as 'this' since our constructor implements the actionlistener interface

        frame1.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame1.setSize(430,430);
        frame1.setLayout(null);
        frame1.add(button1);
        frame1.setVisible(true);
    }

    @Override 
    public void actionPerformed(ActionEvent e) {
        if (e.getSource() == button1) { // checks for whether button was clicked
            frame1.dispose(); // closes out the previous specified frame
            NewWindow window1 = new NewWindow();
        }
    }

}
