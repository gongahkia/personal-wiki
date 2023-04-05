import javax.swing.*;
import javax.swing.JLabel;
import javax.swing.JLabel;
import java.awt.*;
import java.awt.Dimension;
import java.awt.Color;

public class Main {

    public static void main(String[] args) {
        JLabel label1 = new JLabel();
        label1.setOpaque(true);
        label1.setBackground(Color.red);
        label1.setBounds(50,50,200,200);
        
        JLabel label2 = new JLabel();
        label2.setOpaque(true);
        label2.setBackground(Color.blue);
        label2.setBounds(100,100,200,200);

        JLabel label3 = new JLabel();
        label3.setOpaque(true);
        label3.setBackground(Color.green);
        label3.setBounds(150,150,200,200);

        JLayeredPane layered1 = new JLayeredPane();
        layered1.setBounds(0,0,600,600);

        JFrame frame1 = new JFrame();
        layered1.add(label1, JLayeredPane.DEFAULT_LAYER);
        layered1.add(label2, JLayeredPane.DRAG_LAYER);
        layered1.add(label3, Integer.valueOf(0)); // example of integer usage for JLayeredPane
        frame1.add(layered1);

        frame1.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame1.setSize(new Dimension(600,600));
        frame1.setLayout(null);
        frame1.setVisible(true);
    }

}
