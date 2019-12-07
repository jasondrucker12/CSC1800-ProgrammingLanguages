import java.util.*;
import java.lang.*;

public class FamilyTree {
	public static List<Person> head = new ArrayList<Person>();
	public static Map<String, Person> map = new HashMap<String, Person>();
	
	public static void addToFamily(String[] sary) {
		//First pass check if exists in the map
		for(int i = 1; i<sary.length; i++) {
			if(!map.containsKey(sary[i])) {
				map.put(sary[i], new Person(sary[i]));
				if(i==1 && !map.containsKey(sary[i+1])) {   //if person is not in map and spouse is not in map add to heads
					head.add( map.get(sary[i]));
				}
			}
		}
		
		//Second pass set up relations
		for(int j=1; j<sary.length; j++) {
			if(j==1) {  //if first add next name as spouse
				map.get(sary[j]).addSpouse(map.get(sary[j+1]));
				if(sary.length == 4) {
					map.get(sary[j]).children.add(map.get(sary[j+2]));
				}
			}
			if(j==2) {  //if second add previous person as spouse
				map.get(sary[j]).addSpouse(map.get(sary[j-1]));
				if(sary.length == 4) {
					map.get(sary[j]).children.add(map.get(sary[j+1]));
				}
			}
			if(j==3) {  //if third add previous two as your parents
				map.get(sary[j]).parents.add(map.get(sary[j-2]));
				map.get(sary[j]).parents.add(map.get(sary[j-1]));
			}
		}
	}

	//GET COUSINS
	public static void getCousins(Person person, Set<Person> set, int level, boolean modified){
		if(person == null)
			return;
		if(level == 0) {
			getSiblings(person, set);
			return;
		}
		
		List<Person> topAncestors = new ArrayList<Person>();

		Set<Person> familyLineSet = new HashSet<Person>();
		getAncestorsOnLevel(person, topAncestors, level, familyLineSet);

		for(Person p: topAncestors) {
			if(person.parents.size()>0)
				getDescendantsOnLevel(p, set, level, person.parents.get(0), familyLineSet);
		}
		
		if(modified){
			Set<Person> temp = new HashSet<Person>();
			temp.addAll(set);
			
			Set<Person> set2 = new HashSet<Person>();
			getAncestors(person, set2, true);
			for(Person l : set2 ) {
				getCousins(l, set, level, false);
			}
			
			for(Person i: temp)
				getDescendants(i, set);
		}
	}
	
	//creates of all ancestors on a certain level
	public static void getAncestorsOnLevel(Person person, List<Person> list, int level, Set<Person> familyLineSet){
		if(person == null)
			return;
		
		if(level < 0) {
			list.add(person);
			return;
		}
		familyLineSet.add(person);
		if(person.parents.size() == 0) {
			return;
		}
		
		getAncestorsOnLevel(person.parents.get(0), list, level-1, familyLineSet);
		getAncestorsOnLevel(person.parents.get(1), list, level-1, familyLineSet);
	}
	
	public static void getAncestors(Person person, Set<Person> set, boolean isFirst){
		if(person == null)
			return;
		
		if(person.parents.size() == 0) {
			set.add(person);
			return;
		}
		if(!isFirst)
			set.add(person);
		getAncestors(person.parents.get(0), set, false);
		getAncestors(person.parents.get(1), set, false);
	}
	
	public static void getDescendantsOnLevel(Person person, Set<Person> set, int level, Person parentsflag, Set<Person> familyLineSet) {
		if(person == null)
			return;
		
		if(level < 0) {
			if(!(person.parents.contains(parentsflag)))
				set.add(person);
			return;
		}
		for(Person p : person.children) {
			if(!(familyLineSet.contains(p)))
				getDescendantsOnLevel(p, set, level-1, parentsflag, familyLineSet);
		}
	}

	public static void getDescendants(Person person, Set<Person> set) {
		if(person == null)
			return;
		
		set.add(person);
		if(person.children.size() == 0)
			return;
		for( Person p : person.children) {
			getDescendants(p, set);
		}
	}
	
	public static void getSiblings(Person person, Set<Person> set) {
		if(person == null)
			return;
		
		if(person.parents.size()==0)
			return;
		set.addAll(person.parents.get(0).children);
		set.addAll(person.parents.get(1).children);
		set.remove(person);
	}
	
	public static void getUnrelated(Person person, Set<Person> set) {
		for(String s: map.keySet()) {
			set.add(map.get(s));
		}
		
		Set<Person> ancestors = new HashSet<Person>();
		getAncestors(person, ancestors, true);
		
		Set<Person> ancDescendants = new HashSet<Person>();
		for(Person l : ancestors) {
			getDescendants(l, ancDescendants);
		}
		ancestors.addAll(ancDescendants);
		
		for(Person i: ancestors) {
			set.remove(i);
		}
	}
	
	public static void main(String[] args) {
		Scanner scan = new Scanner(System.in);
		
		String str = "";
		while(!str.equals("exit") && scan.hasNext()) {   //TODO: For some reason does not get the last line
			str = scan.nextLine();
			String[] sary = str.split(" ");
			
			if(!(sary.length == 3 || sary.length == 4 || sary.length == 5))
				continue;
				
			String flag = sary[0];
	
			Set<Person> dataset = new HashSet<Person>();
			
			switch(flag) {
				case("E"): 
					addToFamily(sary);
					break;
				case("W"): 
					Person person = map.get(sary[2]);
					switch(sary[1]) {
						case("child"):
							dataset.addAll(person.children);
							break;
						case("sibling"):
							getSiblings(person, dataset);
							break;
						case("ancestor"):
							getAncestors(person, dataset, true);
							break;
						case("cousin"):
							int num = 1;
							if(Character.isDigit(sary[2].charAt(0))){
								person = map.get(sary[3]);
								num = Integer.valueOf(sary[2]);
							}
							getCousins(person, dataset, num, true);
							break;
						case("unrelated"):
							getUnrelated(person,dataset);
							break;
					}
					System.out.println("\n" + str);
					for(Person pers: dataset) {
						System.out.println(pers.name);
					}
					break;
				case("X"): 
					Person person1 = map.get(sary[1]);
					Person person2 = map.get(sary[3]);
					boolean val = false;
					switch(sary[2]) {
						case("child"):
							val = person2.children.contains(person1);
							break;
						case("sibling"):
							getSiblings(person2, dataset);
							val = dataset.contains(person1);
							break;
						case("ancestor"):
							getAncestors(person2, dataset, true);
							val = dataset.contains(person1);
							break;
						case("cousin"):
							int num = 1;
							if(Character.isDigit(sary[3].charAt(0))){
								person2 = map.get(sary[4]);
								num = Integer.valueOf(sary[3]);
							}
							getCousins(person2, dataset, num, true);
							val = dataset.contains(person1);
							break;
						case("unrelated"):
							getUnrelated(person2, dataset);
							val = dataset.contains(person1);
							break;
					}
				String valstring = (val) ? "Yes" : "No";
				System.out.println("\n" + str);
				System.out.println(valstring);	
				break;
			}
		}
		scan.close();
	}
}


