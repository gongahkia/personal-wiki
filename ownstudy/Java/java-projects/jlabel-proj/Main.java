import javax.swing.JFrame;
import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.ImageIcon;
import javax.swing.border.Border;
import java.awt.Color;
import java.awt.Font;

public class Main {

    public static void main(String[] args) {

        ImageIcon sadCat = new ImageIcon("download.jpeg"); // loads in the image asset within the specified file path
        Border border1 = BorderFactory.createLineBorder(Color.green,3); // creates a border instance object that wraps the JFrame label that is created

        JLabel label1 = new JLabel(); // we can also instantiate label object by providing it a text argument
        label1.setText("Shitass"); // set text for the created label object
        label1.setIcon(sadCat); // sets image for the created label object
        label1.setVerticalTextPosition(JLabel.TOP);
        label1.setHorizontalTextPosition(JLabel.CENTER); // allows us to set the vertical and horizontal location of items in our JFrame using JLabel
        label1.setForeground(Color.ORANGE); // sets text color
        label1.setFont(new Font("Comic Sans", Font.PLAIN, 20)); // customizes font size and family for text
        label1.setIconTextGap(200); // self-explanatory
        label1.setBackground(Color.black); // sets background color
        label1.setOpaque(true); // has to be accomponied with setBackground() method to make background visible
        label1.setBorder(border1); // sets border for the created label
        label1.setVerticalAlignment(JLabel.CENTER); // sets vertical alignment of the entire label at once (instead of individually as in the .setVerticalTextPosition method called previously
        label1.setHorizontalAlignment(JLabel.CENTER); // sets horizonatl alignment of the entire label
        // label1.setBounds(0,0,600,600); // specifies the cropped area of the window that we want to see our label in

        JFrame frame1 = new JFrame();
        frame1.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        // frame1.setExtendedState(JFrame.MAXIMIZED_BOTH); // an alternative to .setSize() method that maximizes the app window by default
        // frame1.setLayout(null); // specifies we do not want to use the default, allowing us to configure the desired horizontal and vertical bounds accordingly
        frame1.setVisible(true);
        frame1.add(label1); // adds different components to our JFrame
        frame1.pack(); // frame size will dynamically adjust to fit all components stored within it, this method must be added last to accomodate for all the existing UI element we are adding to the application
    }

}
