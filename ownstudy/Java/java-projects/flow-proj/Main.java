import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JButton;
import java.awt.FlowLayout;
import java.awt.Dimension;
import java.awt.Color;

public class Main {
    
    public static void main(String[] args) {
        JFrame frame1 = new JFrame();
        frame1.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame1.setSize(500,500);
        frame1.setLayout(new FlowLayout(FlowLayout.TRAILING)); // the default empty argument is set to FlowLayout.CENTER, .LEADING will give a top left formatted layout

        JPanel panel1 = new JPanel();
        panel1.setPreferredSize(new Dimension(250,250));
        panel1.setBackground(Color.lightGray);
        panel1.setLayout(new FlowLayout());

        panel1.add(new JButton("1"));
        panel1.add(new JButton("2"));
        panel1.add(new JButton("3"));
        panel1.add(new JButton("4"));
        panel1.add(new JButton("5"));
        panel1.add(new JButton("6"));
        panel1.add(new JButton("7"));
        panel1.add(new JButton("8"));
        panel1.add(new JButton("9"));
        panel1.add(new JButton("10"));
        frame1.add(panel1);
        frame1.setVisible(true);
    }

}
