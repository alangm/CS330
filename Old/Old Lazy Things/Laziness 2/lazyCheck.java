import java.io.*;

public class lazyCheck {
	public static void main(String[] args){
		int a = 1;
		int b = 2;
		stateChange(++a, ++b);
		if(b==2){
			System.out.println("lazy");
		}
		else {
			System.out.println("eager");
		}
	}

	public static void stateChange(int change, int same){
		System.out.println(change);
	}
}
		
