import javax.swing.JFrame;
import javax.swing.ImageIcon;
import java.awt.Color;

public class Main {

    public static void main(String[] args) {
        JFrame frame1 = new JFrame(); // creates a new instance of the JFrame object
        frame1.setTitle("shitass"); // sets title name of the JFrame
        frame1.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE); // JFrame object closes on pressing the 'X' button, opposed to the HIDE_ON_CLOSE method
        
        frame1.setResizable(false); // prevent frame from being resized
        frame1.setSize(420,420); // sets X-dimension and Y-dimension of the JFrame
        frame1.setVisible(true);

        ImageIcon annoyingOrange = new ImageIcon("unnamed.jpg"); // import logo to be added to the top left of Java project
        frame1.setIconImage(annoyingOrange.getImage()); // sets our annoyingOrange image as the icon at top of JFrame
        
        frame1.getContentPane().setBackground(Color.cyan); // sets background color, we can create custom colors as well using Hexadecimal representation of color/ (255,255,255) like in CSS
    }

}
