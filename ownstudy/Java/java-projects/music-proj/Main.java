import java.io.File;
import java.util.Scanner;
import java.io.IOException;
import javax.sound.sampled.*;

public class Main {

    public static void main(String[] args) throws UnsupportedAudioFileException, IOException, LineUnavailableException { // throws
                                                                                                                         // are
                                                                                                                         // an
                                                                                                                         // alternative
                                                                                                                         // to
                                                                                                                         // try
                                                                                                                         // and
                                                                                                                         // catch
                                                                                                                         // blocks,
                                                                                                                         // which
                                                                                                                         // preempt
                                                                                                                         // a
                                                                                                                         // possible
                                                                                                                         // crash
                                                                                                                         // of
                                                                                                                         // the
                                                                                                                         // program
                                                                                                                         // by
                                                                                                                         // listing
                                                                                                                         // all
                                                                                                                         // the
                                                                                                                         // possible
                                                                                                                         // exception
                                                                                                                         // errors
                                                                                                                         // to
                                                                                                                         // arise

        File fhand = new File("poop.wav");
        AudioInputStream audiostream = AudioSystem.getAudioInputStream(fhand);
        Clip clip = AudioSystem.getClip();
        clip.open(audiostream);

        String response = "";

        while (!response.equals("Q")) {
            Scanner iphand = new Scanner(System.in);
            response = iphand.next();
            response = response.toUpperCase();
            System.out.println("Q = quit");
            System.out.print("Enter your choice: ");

            if (response == "Q") {
                clip.close();
                break;
            }
        }

        clip.start();
    }

}