#include <iostream>
#include <string>
#include <cstring>
#include <cstdlib>
#include <cstdio>
#include <sstream>
#include <algorithm>
#include <map>
#include <vector>
#include <utility>

using namespace std;

struct node {
	std::vector<struct node*> parents;
	std::map<string, double> probability_table;
	string name;
};

void tree(map<string, struct node*> &nodes, string buffer) {
  
  string builder;
  getline(cin, buffer);

  for(int i = 0; i < buffer.length(); i++){
    if(buffer[i]==',') {
      nodes[builder] = new node;
			nodes[builder]->name = builder;
			builder="";
    } else if(i == (buffer.length() - 1)) {
			builder += buffer[i];
			nodes[builder] = new node;
			nodes[builder]->name = builder;
    } else if(buffer[i]==' ') {
			builder="";
		} else {
			builder += buffer[i];
		}
	}
}

void bayes(map<string, struct node*> &nodes, string buffer, int number) {
	string builder, aux;
	char devourer;
	double readerProb;
	bool bandera = true;
	int out=0;

	for(out=0; out<number;out++) {
		
    getline(cin, buffer);
    
		builder = "";
		bool indexNode = false;
    for(int i = 0; i < buffer.length(); i++) {
    	if(buffer[i] == '|'){
			indexNode = true;
			aux = builder;
			builder="";
		}
	    if(buffer[i] == '+' || buffer[i] == '-' || buffer[i] == ' ' 
	         || buffer[i] == '=' || buffer[i] == ',' || buffer[i] == '|' || 
	         (buffer[i] >= 48 && buffer[i] <= 57));
		else{
		    builder += buffer[i];
		}
		if(indexNode) {
	        if(buffer[i] == ',') {
	        	(nodes[aux])->parents.push_back(nodes[builder]);
				builder="";
	        } else if (buffer[i] == '=') {
				(nodes[aux])->parents.push_back(nodes[builder]);
				builder="";
			}
		}
		if(buffer[i] == '=') {
	        if(!indexNode){
				aux=builder;
			}
			break;
		}
		
		
		}


    	sort( (nodes[aux])->parents.begin(), (nodes[aux])->parents.end() );
		(nodes[aux])->parents.erase( unique( (nodes[aux])->parents.begin(), (nodes[aux])->parents.end() ), (nodes[aux])->parents.end() );

		builder="";
		for(int i = 0; i < buffer.length(); i++) {
			if(buffer[i] == '=') {
				break;
			} else if(buffer[i] == ' ' && buffer[i + 1] == '=');
			else {
				builder+=buffer[i];
			}
		}

		if(builder.compare("") != 0) {
			stringstream extract(buffer);
			do {
				extract >> devourer;
			} while(devourer!='=');

			extract >> readerProb;

			(nodes[aux])->probability_table[builder]=readerProb;
			builder[0]='-';
			(nodes[aux])->probability_table[builder]=1 - readerProb;
		}
	}
}

string hidden_nodes(map<string, struct node*> nodes, string key_node) {

  string builder = "";

  if(nodes[key_node]->parents.empty()) {
		builder += key_node;
	} else {
		for(int i = 0; i < nodes[key_node]->parents.size(); i++) {
			builder += hidden_nodes(nodes, nodes[key_node]->parents[i]->name) + " ";
		}
		builder += key_node;
	}
	return builder;
}

string prepare(string buffer){
	string auxiliar = "";
	for(int i =0; i< buffer.length(); i++){
		if(buffer[i]==','){
			if(buffer[i+1]!= ' '){
				auxiliar+=buffer[i];
				auxiliar+=" ";
			}else{
				auxiliar+=buffer[i];
			}
		}else{
			auxiliar+=buffer[i];
		}
	}
	
	return auxiliar;

}

double chain_rule(map<string, struct node*> nodes,string chain_probability){

  string builder;
	char signs[] = "+-";
	std::vector<string> index, statements, probs;
	chain_probability.erase (std::remove(chain_probability.begin(), chain_probability.end(), ','), chain_probability.end());
	stringstream extract(chain_probability);
	double acum = 1.0;

  while(extract >> builder) {
		statements.push_back(builder);
		for (int i = 0; i < strlen(signs); i++) {
	      builder.erase (std::remove(builder.begin(), builder.end(), signs[i]), builder.end());
	  	}
	  	index.push_back(builder);
	}
	for (int i = 0; i < index.size(); i++) {
		std::map<string, double> copyMap = nodes[index[i]]->probability_table;
		for (std::map<string,double>::iterator ite = copyMap.begin(); ite !=  copyMap.end(); ++ite) {
			if (ite->first.find(statements[i]) != std::string::npos) {
			    probs.push_back(ite->first);
			}
		}
		if(!nodes[index[i]]->parents.empty()) {
			int dependeces = nodes[index[i]]->parents.size();
			int indexProbs;
			for(int j = 0; j < probs.size(); j++){
				int count = 0;
				for (int k = 0; k < statements.size(); k++) {
					if(probs[j].find(statements[k]) != std::string::npos){
						count++;
					}
				}
				if(count == (dependeces + 1)) {
					indexProbs = j;
					break;
				}
			}
			acum *= copyMap[probs[indexProbs]];
		} else {
			acum *= copyMap[probs[0]];
		}
		while(!probs.empty()){
			probs.pop_back();
		}
	}
	return acum;
}

void expand(string buffer, vector<string> relevant_nodes, vector<string> &sum_expansion) {

  string builder1, builder2;

  if(relevant_nodes.size() == 1) {
		builder1 = buffer + ", +" + relevant_nodes[0];
		sum_expansion.push_back(builder1);
		builder2 = buffer + ", -" + relevant_nodes[0];
		sum_expansion.push_back(builder2);
	} else {
		builder1 = buffer  + ", +" + relevant_nodes.back();
		builder2 = buffer  + ", -" + relevant_nodes.back();
		relevant_nodes.pop_back();
		expand(builder1, relevant_nodes, sum_expansion);
		expand(builder2, relevant_nodes, sum_expansion);
	}
}

double joint(map<string, struct node*> nodes, string buffer) {

  string builder,aux,picker,hidden = "";
	double acum = 0.0;

  for(int i = 0; i < buffer.length(); i++) {
		if(buffer[i] == ',') {
			aux = hidden_nodes(nodes, builder);
			builder = "";
		}
		if(buffer[i] == '+' || buffer[i] == '-' || buffer[i] == ' ' || buffer[i] == ',');
		else{
			builder += buffer[i];
		}

		stringstream extract(aux);

		while(extract >> picker){
			if(hidden.find(picker) != string::npos);
			else{
				hidden += picker + " ";
			}
		}
	}
	aux = hidden_nodes(nodes, builder);
	builder = "";

	stringstream extract1(aux);

	while(extract1 >> picker){
		if(hidden.find(picker) != string::npos);
		else{
			hidden += picker + " ";
		}
	}

	stringstream extract2(hidden);

	std::vector<string> relevant_nodes;

	while(extract2 >> picker){
		if(buffer.find(picker) != string::npos);
		else{
			builder += picker + " ";
			relevant_nodes.push_back(picker);
		}
	}

	vector<string> sum_expansion;

	if(!relevant_nodes.empty()) {
		expand(buffer, relevant_nodes, sum_expansion);
		for(int i = 0; i < sum_expansion.size(); i++) {
			acum += chain_rule(nodes, sum_expansion[i]);
		}
	} else {
		acum = chain_rule(nodes, buffer);
	}
	return acum;
}

void query(map<string, struct node*> nodes, string buffer){

	double res;
	string numerator, denominator;
	numerator = "";
	denominator = "";
	
	bool dependece = false;
	buffer = prepare(buffer);

  for(int i = 0; i < buffer.length(); i++){
  	   for(int w=0; w<buffer.length();w++){
  	   	
  	   }
		if(buffer[i] == '|') {
			dependece = true;
			numerator += ", ";
		}
		if(!dependece) {
			if(buffer[i] != '|') {
				numerator += buffer[i];
			}
		} else {
			if(buffer[i] != '|') {
				numerator += buffer[i];
				denominator += buffer[i];
			}
		}
	}
	if(denominator.compare("") == 0){
		res = joint(nodes, numerator);
	}else{
		res = joint(nodes,numerator) / joint(nodes,denominator);
	}

	printf("%.5G\n", res);
}


int main(int argc, char *argv[]) {

	std::map<string, struct node*> nodes;
	string buffer;
	int num = 0;

	tree(nodes,buffer);
	getline(cin,buffer);
	num=atoi(buffer.c_str());
	bayes(nodes,buffer,num);
	
	getline(cin,buffer);
	num=atoi(buffer.c_str());
	
	int i = 0;
	for(i=0; i<num; i++){
		getline(cin, buffer);
		query(nodes, buffer);
	}
	return 0;
}