/*
g++ model_3.cpp -Wall -Wextra -o3 -std=c++17 -o model_3
./model_2 init_parent_pair_number_0 1 sampled_child_number_0 1
sampled_father_number_0 1  sampled_mother_number_0 1
migration_number lambda_0 lambda_1 flag_constant  flag_invasive-sampling (total 13 parameters)
./model_2 10 10 3 3 2 2 2 2 3 3 5 0 1
*/
#include <iostream>
#include <fstream>
#include <vector>
#include <utility>
#include <random>
#include <chrono>
#include <string>
#include <algorithm>
#include <numeric>

std::random_device rd;
const auto seed = rd();
std::mt19937 rng(seed);

template <class T> inline
T cumsum(const T& v) {
  T result(v.size());
  std::partial_sum(v.begin(), v.end(), result.begin());
  return result;
}

class Individual
{
private:
  static int LATEST_ID;
  const int id;
  const std::pair<int, int> parents_ids;
public:
  Individual() :
  id(LATEST_ID++){
  };
  Individual(const int father_id, const int mother_id) :
  id(LATEST_ID++),
  parents_ids(father_id, mother_id) {
  };
  int get_id() const {
    return id;
  }
  const std::pair<int, int> get_parent_ids() const {
    return parents_ids;
  }
  void print_id() const {
    std::cout << id << std::endl;
  }
  void print_parents_id() const {
    std::cout << id << "\t" << parents_ids.first  << "\t" << parents_ids.second << std::endl;
  }
  std::ostream& write(std::ostream& ost) const {
    return ost << id << "\t" << parents_ids.first << "\t" << parents_ids.second;
  }
};
std::ostream& operator<<(std::ostream& ost, const Individual& x) {
  return x.write(ost);
}

class Population
{
private:
  std::vector<Individual> fathers;
  std::vector<Individual> mothers;
  std::vector<Individual> children;
  bool debug = true;
public:
  Population(const size_t init_parent_number) :
  fathers(init_parent_number),
  mothers(init_parent_number) {
  };
  size_t roulette_selection(const std::vector<double> cumsum_weight) const {
      std::uniform_real_distribution<> uniform_real(0, cumsum_weight.back());
      double r = uniform_real(rng);
      auto it = std::upper_bound(cumsum_weight.begin(), cumsum_weight.end(), r);
      return (it - cumsum_weight.begin());
  }
  void reproduction_constant(const double lambda) {
    const size_t father_number = fathers.size();
    const size_t mother_number = mothers.size();
    std::uniform_int_distribution<size_t> uniform_int(0, father_number - 1);
    std::poisson_distribution<size_t> poisson(lambda);
    if (debug) std::cerr << "fathers.size() = " << fathers.size() << ", mothers.size() = " << mothers.size() << std::endl;
    for (size_t i = 0; i < mother_number; ++i) {
      const size_t child_number = poisson(rng);
      if (debug) std::cerr << "child_number = " << child_number << std::endl;
      for (size_t j = 0; j < child_number; ++j) {
        const size_t father_index = uniform_int(rng);
        children.push_back( Individual( fathers[father_index].get_id(), mothers[i].get_id() ) );
        if (debug) children.back().print_parents_id();
      }
    }
  }
  void reproduction(const double lambda_mean) {
    const size_t father_number = fathers.size();
    const size_t mother_number = mothers.size();
    std::vector<double> fathers_lambda(father_number);
    //std::uniform_int_distribution<size_t> uniform_int(0, father_number - 1);
    std::exponential_distribution<> exponential(1/lambda_mean);
    std::geometric_distribution<size_t> geometric(1/(1+lambda_mean)); // Poisson reproduction with pramater lambda following exponential (including 0)
    if (debug) std::cerr << "fathers.size() = " << fathers.size() << ", mothers.size() = " << mothers.size() << std::endl;
    for (size_t i = 0; i < father_number; ++i) {
      fathers_lambda[i] = exponential(rng);
    }
    const std::vector<double> cumsum_fathers_lambda = cumsum(fathers_lambda);
    for (size_t i = 0; i < father_number; ++i) {
      //if (debug) std::cout << "cumsum_fathers_lambda[i] = " << cumsum_fathers_lambda[i] << std::endl;
    }
    for (size_t i = 0; i < mother_number; ++i) {
      //if (debug) std::cout << "mother_id: " << i << "\tmothers[i].get_id(): " << mothers[i].get_id() << std::endl;
      const size_t child_number = geometric(rng);
      if (debug) std::cerr << "child_number = " << child_number << std::endl;
      for (size_t j = 0; j < child_number; ++j) {
        std::uniform_real_distribution<> uniform_real(0, cumsum_fathers_lambda.back());
        double r = uniform_real(rng);
        //if (debug) std::cout << "r = " << r << std::endl;
        size_t father_index = 0;
        while (father_index < cumsum_fathers_lambda.size()) {
          if(r < cumsum_fathers_lambda[father_index]) {
            if (debug) {
              //std::cout << "father_id: " << father_index << "\tfathers[father_index].get_id(): " << fathers[father_index].get_id() << std::endl;
            }
            break ;
          }
          father_index++;
        }
        children.push_back( Individual( fathers[father_index].get_id(), mothers[i].get_id() ) );
        if (debug) children.back().print_parents_id();
      }
    }
  }
  void print_family_size() {
    std::cerr << "fathers.size() = " << fathers.size() << ", mothers.size() = " << mothers.size() << ", children.size() = " << children.size() << std::endl;
  }
  void print_family_id() {
    for (size_t i = 0; i < children.size(); ++i) {
      children[i].print_parents_id();
    }
  }
  bool check_size() {
    return fathers.empty() || mothers.empty();
  }
  void sampling_child(const size_t sampled_number, size_t rep) {
    std::vector<size_t> sampled_ids;
    std::vector<size_t> all_indices(children.size());
    std::iota(all_indices.begin(), all_indices.end(), 0);
    std::sample(all_indices.begin(), all_indices.end(), std::back_inserter(sampled_ids), sampled_number, rng);
    if (debug) std::cout << "sampled_child & its parents_id: " << std::endl;
    std::string filename = "sample_child.txt";
    filename = std::to_string(rep) + filename;
    std::ofstream writing_sample;
    writing_sample.open(filename, std::ios::out);
    writing_sample << "id\tfather\tmother" << "\n";
    for (const auto& i: sampled_ids) {
      if (debug) children[i].print_parents_id();
      writing_sample << children[i] << "\n";
    }
  }
  void sampling_father(const size_t sampled_number, size_t rep, size_t flag_invasive) {
    std::vector<size_t> sampled_ids;
    std::vector<size_t> all_indices(fathers.size());
    std::vector<Individual> next_fathers;
    std::iota(all_indices.begin(), all_indices.end(), 0);
    std::sample(all_indices.begin(), all_indices.end(), std::back_inserter(sampled_ids), sampled_number, rng);
    if (debug) std::cout << "sampled_father_id: " << std::endl;
    std::string filename = "sample_father.txt";
    filename = std::to_string(rep) + filename;
    std::ofstream writing_sample;
    writing_sample.open(filename, std::ios::out);
    writing_sample << "id" << "\n";
    for (const auto& i: sampled_ids) {
      if (debug) fathers[i].print_id();
      writing_sample << fathers[i].get_id() << "\n";
    }
  }
  void sampling_mother(const size_t sampled_number, size_t rep, size_t flag_invasive) {
    std::vector<size_t> sampled_ids;
    std::vector<size_t> all_indices(mothers.size());
    std::iota(all_indices.begin(), all_indices.end(), 0);
    std::sample(all_indices.begin(), all_indices.end(), std::back_inserter(sampled_ids), sampled_number, rng);
    if (debug) std::cout << "sampled_mother_id: " << std::endl;
    std::string filename = "sample_mother.txt";
    filename = std::to_string(rep) + filename;
    std::ofstream writing_sample;
    writing_sample.open(filename, std::ios::out);
    writing_sample << "id" << "\n";
    for (const auto& i: sampled_ids) {
      if (debug) mothers[i].print_id();
      writing_sample << mothers[i].get_id() << "\n";
    }
  }
  std::vector<size_t> return_migrant_ids(std::vector<Individual> migrant_parents, const size_t migrant_number) {
    std::vector<size_t> migrant_ids;
    std::vector<size_t> all_indices(migrant_parents.size());
    std::iota(all_indices.begin(), all_indices.end(), 0);
    std::sample(all_indices.begin(), all_indices.end(), std::back_inserter(migrant_ids), migrant_number, rng);
    return migrant_ids;
  }
  void migration(Population pop, const size_t migrant_number) {
    std::vector<size_t> migrant_father_ids = return_migrant_ids(pop.fathers, migrant_number);
    std::vector<size_t> migrant_mother_ids = return_migrant_ids(pop.mothers, migrant_number);
    for (const auto& i: migrant_father_ids) {
      if (debug) std::cout << "migrant_father_ids: " << pop.fathers[i].get_id() << std::endl;
      fathers.push_back(pop.fathers[i]);
    }
    for (const auto& i: migrant_mother_ids) {
      if (debug) std::cout << "migrant_mother_ids: " << pop.mothers[i].get_id() << std::endl;
      mothers.push_back(pop.mothers[i]);
    }
  }
};

int Individual::LATEST_ID = 0;
bool debug = false;
int main(int argc, char *argv[])
{
  std::chrono::system_clock::time_point start, end;
  start = std::chrono::system_clock::now();
  if ( argc != 14 ) {
    fprintf(stderr,"Command line arguments are incorrect\n");
    return 0;
  }
  const size_t init_parent_number_0 = atoi(argv[1]); //1 this number equal to father&mother number (i.e., multipled twice for total parents number)
  const size_t init_parent_number_1 = atoi(argv[2]); //2
  const size_t sampled_child_number_0 = atoi(argv[3]); //3
  const size_t sampled_child_number_1 = atoi(argv[4]); //4
  const size_t sampled_father_number_0 = atoi(argv[5]); //5
  const size_t sampled_father_number_1 = atoi(argv[6]); //6
  const size_t sampled_mother_number_0 = atoi(argv[7]); //7
  const size_t sampled_mother_number_1 = atoi(argv[8]); //8
  const size_t migrant_number = atof(argv[9]); //9 assuming same migrant number among father&mother
  const double lambda_mean_0 = atof(argv[10]);//10
  const double lambda_mean_1 = atof(argv[11]);//11
  const int flag_constant = atof(argv[12]);//12
  const int flag_invasive = atof(argv[13]);//13

  if ( migrant_number > init_parent_number_0 ) {
    fprintf(stderr,"migrant_number must be smaller than init_parent_number\n");
    return 0;
  }
  if (debug) std::cout << "INPUT: parent_pair_size = " << init_parent_number_0 << "," << init_parent_number_1 << ", sampled_child_number = " << sampled_child_number_0 << "," << sampled_child_number_1 << ", sampled_father_number = " << sampled_father_number_0 << "," << sampled_father_number_1 << ", sampled_mother_number = " << sampled_mother_number_0 << "," << sampled_mother_number_1 << ", migrant_number = " << migrant_number << ", lambda_mean_0 = " << lambda_mean_0 << ", and lambda_mean_1 = " << lambda_mean_1 << std::endl;

  Population pop0(init_parent_number_0);
  if (flag_constant) {
    pop0.reproduction_constant(lambda_mean_0);
  } else{
    pop0.reproduction(lambda_mean_0);
  }
  pop0.sampling_child(sampled_child_number_0, 0);
  pop0.sampling_father(sampled_father_number_0, 0, flag_invasive);
  pop0.sampling_mother(sampled_father_number_0, 0, flag_invasive);

  Population pop1(init_parent_number_1 - migrant_number);
  pop1.migration(pop0, migrant_number);
  if (flag_constant) {
    pop1.reproduction_constant(lambda_mean_1);
  } else{
    pop1.reproduction(lambda_mean_1);
  }
  pop1.sampling_child(sampled_child_number_1, 1);
  pop1.sampling_father(sampled_father_number_1, 1, flag_invasive);
  pop1.sampling_mother(sampled_father_number_1, 1, flag_invasive);
  if (debug) std::cerr << "-----------------------" << std::endl;
  end = std::chrono::system_clock::now();
  double elapsed_time = std::chrono::duration_cast<std::chrono::microseconds>(end-start).count();
  if (debug) std::cerr << "elapsed_time = " << elapsed_time/1000/1000 << " seconds" << std::endl;
}
