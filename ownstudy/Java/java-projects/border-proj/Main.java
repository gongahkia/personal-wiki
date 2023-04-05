import javax.swing.JFrame;
import javax.swing.JPanel;
import java.awt.Dimension;
import java.awt.BorderLayout;
import java.awt.Color;

public class Main {

    public static void main(String[] args) {
        JFrame frame1 = new JFrame();
        frame1.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame1.setSize(500, 500);
        frame1.setLayout(new BorderLayout(10,10)); // specifying arguments within the BorderLayout() method gives more or less border margins between each panel
        frame1.setVisible(true);
        
        JPanel panel1 = new JPanel();
        JPanel panel2 = new JPanel();
        JPanel panel3 = new JPanel();
        JPanel panel4 = new JPanel();
        JPanel panel5 = new JPanel();

        panel1.setBackground(Color.red);
        panel2.setBackground(Color.blue);
        panel3.setBackground(Color.green);
        panel4.setBackground(Color.black);
        panel5.setBackground(Color.orange);

        panel1.setPreferredSize(new Dimension(100,100));
        panel2.setPreferredSize(new Dimension(100,100));
        panel3.setPreferredSize(new Dimension(100,100));
        panel4.setPreferredSize(new Dimension(100,100));
        panel5.setPreferredSize(new Dimension(100,100));

        frame1.add(panel1, BorderLayout.NORTH); // note that .add() method can be called on other panels to create nested and sub panels
        frame1.add(panel2, BorderLayout.EAST);
        frame1.add(panel3, BorderLayout.WEST);
        frame1.add(panel4, BorderLayout.SOUTH);
        frame1.add(panel5, BorderLayout.CENTER);
    }

}
