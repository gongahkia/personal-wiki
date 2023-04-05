import javax.swing.JFrame;
import javax.swing.JButton;
import java.awt.GridLayout;

public class Main {

    public static void main(String[] args) {
        JFrame frame1 = new JFrame();
        frame1.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame1.setSize(600,600);
        frame1.setLayout(new GridLayout(3,3,10,100)); // default argument taken in assumes 1 row, argument to be passed in in the format of (X rows, Y columns)

        frame1.add(new JButton("Button 1")); // passing an anonymous object button to a function since the exact identity isn't really important
        frame1.add(new JButton("Button 2"));
        frame1.add(new JButton("Button 3"));
        frame1.add(new JButton("Button 4"));
        frame1.add(new JButton("Button 5"));
        frame1.add(new JButton("Button 6"));
        frame1.add(new JButton("Button 7"));
        frame1.add(new JButton("Button 8"));
        frame1.add(new JButton("Button 9"));
        
        frame1.setVisible(true);
    }

}
