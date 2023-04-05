import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JLabel;
import javax.swing.ImageIcon;
import javax.swing.border.Border;
import java.awt.Color;

public class Main {

    public static void main(String[] args) {
        ImageIcon icon1 = new ImageIcon("download.png");

        JLabel label1 = new JLabel();
        label1.setText("hi");
        label1.setIcon(icon1);
        label1.setVerticalAlignment(JLabel.TOP);
        label1.setHorizontalAlignment(JLabel.RIGHT);
        label1.setBounds(0,0,75,75);

        JPanel panel1 = new JPanel();
        panel1.setBackground(Color.blue);
        panel1.setBounds(0,0,250,250); // we must declare bounds when no layout manager is used
        panel1.setLayout(null);
        
        JPanel panel2 = new JPanel();
        panel2.setBackground(Color.red);
        panel2.setBounds(250,0,250,250);
        panel2.setLayout(null);

        JPanel panel3 = new JPanel();
        panel3.setBackground(Color.green);
        panel3.setBounds(0,250,500,250);
        panel3.setLayout(null);

        JFrame frame1 = new JFrame();
        frame1.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame1.setLayout(null);
        frame1.setSize(750,750);
        frame1.setVisible(true);
        panel1.add(label1);
        frame1.add(panel1);
        frame1.add(panel2);
        frame1.add(panel3);

    }

}
