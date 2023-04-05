import javax.swing.JFrame;
import javax.swing.JButton;
import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import java.awt.Color;
import java.awt.Font;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;


public class frame1 extends JFrame implements ActionListener{ // child class inheriting its parent's class attributes and values, as well as implementing the interface ActionListener
        
    JButton button1; // declare this outside the constructor statement as a global variable that can be accessed by methods

    frame1() { // constructor statement
        
        ImageIcon icon1 = new ImageIcon("cute.jpg");

        button1 = new JButton();
        button1.setBounds(200,100,250,100);
        button1.addActionListener(this); // action listener checks for an update on whether a specified action has occured
        // button1.addActionListener(e -> System.out.println("poo")); achieves the same effect as the above code, except that it is done with less syntax, all through the use of lambda expressions
        
        button1.setText("ah shit");
        button1.setFocusable(false);
        button1.setIcon(icon1);
        button1.setHorizontalTextPosition(JButton.CENTER);
        button1.setVerticalTextPosition(JButton.BOTTOM);
        button1.setFont(new Font("Sans Serif", Font.BOLD, 25));
        button1.setIconTextGap(-15);
        button1.setForeground(Color.pink);
        button1.setBackground(Color.lightGray);
        button1.setBorder(BorderFactory.createEtchedBorder());

        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        this.setLayout(null);
        this.setSize(500,500);
        this.setVisible(true);
        this.add(button1);
    }
    
    @Override
    public void actionPerformed(ActionEvent e) {
        if (e.getSource() == button1) {
            System.out.println("poo");
        }
    }
}
