import javax.swing.JOptionPane;

public class Main {

    public static void main(String[] args) {
        JOptionPane dialog1 = new JOptionPane();
        dialog1.showMessageDialog(null, "shit ass", "title", JOptionPane.PLAIN_MESSAGE); // note the other possible customizations in Java.md
        
        JOptionPane.showConfirmDialog(null, "good morning people", "title", JOptionPane.YES_NO_CANCEL_OPTION); // note the other possible customizations in Java.md

        String userInput = JOptionPane.showInputDialog("What's good my slimes?\n:");
        System.out.println("Aignt" + userInput);

        JOptionPane.showOptionDialog(null, "You are awesome!", "secret message", JOptionPane.YES_NO_CANCEL_OPTION, 0, null, null, 0); // read the oracle Java documentation for the multiple parameters .showOptionDialog() method takes in
    }

}
