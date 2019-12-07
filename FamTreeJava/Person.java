import java.util.*;

public class Person {
	public String name;
	public List<Person> parents;
	public List<Person> children;
	public List<Person> spouse;
	
	public Person(String name) {
		this.name = name;
		spouse = new ArrayList<Person>();
		parents = new ArrayList<Person>();
		children = new ArrayList<Person>();
	}
	
	public void addSpouse(Person s) {
		if(!spouse.contains(s))
			spouse.add(s);
	}

}
