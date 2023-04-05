import javax.swing.JFrame;
import javax.swing.ImageIcon;
import java.awt.Color;

    class myFrame extends JFrame {

        myFrame() { // defaults of the myFrame() class
            this.setTitle("Defaults");
            this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            this.setResizable(false);
            this.setSize(420,420);
            this.setVisible(true);
            ImageIcon icon = new ImageIcon("../jframe-proj/unnamed.jpg");
            this.setIconImage(icon.getImage());
            this.getContentPane().setBackground(new Color(123,60,250));
        }

    }

public class Main {

    public static void main(String[] args) {
        myFrame frame2 = new myFrame(); // instantiates an instance of the myFrame parent class
    }

}
