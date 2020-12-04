import java.io.BufferedReader;
import java.io.FileReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Part1{

    private static long processLine(String line){
        //System.out.println(">>> " + line);
        Pattern p = Pattern.compile("(\\d+)-(\\d+) (\\w): (\\w+)");
        Matcher m = p.matcher(line);
        if (m.matches()){
            int lowerBound = Integer.parseInt(m.group(1));
            int upperBound = Integer.parseInt(m.group(2));
            char key = m.group(3).charAt(0);
            String password = m.group(4);
            //System.out.println(lowerBound + " " + upperBound + " " + key + " " + password);

            long nbOcc = password.chars().filter(c -> c == key).count();

            if (nbOcc <= upperBound && nbOcc >= lowerBound) return 1;
            return 0;
        }
        return 0;
    }

    public static void main(String args[]){
        long correctLines = 0;
        BufferedReader reader;

        try{
            reader = new BufferedReader(new FileReader("./input.txt"));
            String line = reader.readLine();
            while (line != null){
                correctLines += processLine(line);
                line = reader.readLine();
            }
            reader.close();
            System.out.println(correctLines);
        } catch (Exception e){
            e.printStackTrace();
        }
    }
}