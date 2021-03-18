/*
g++ model_1.cpp -Wall -Wextra -o3 -std=c++17 -o model_1
./model_1 init_parent_pair_number sampled_number migration_number lambda_1 lambda_2
./model_1 10 3 2 3 10
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
  bool print_samples_flag = false;
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
  void sampling(const size_t sampled_number, size_t rep) {
    std::vector<size_t> sampled_ids;
    std::vector<size_t> all_indices(children.size());
    std::iota(all_indices.begin(), all_indices.end(), 0);
    std::sample(all_indices.begin(), all_indices.end(), std::back_inserter(sampled_ids), sampled_number, rng);
    if (debug) std::cout << "sampled_child & its parents_id: " << std::endl;
    std::string filename = "sample.txt";
    filename = std::to_string(rep) + filename;
    std::ofstream writing_sample;
    if (print_samples_flag) {
      writing_sample.open(filename, std::ios::app);
    } else {
      writing_sample.open(filename, std::ios::out);
      writing_sample << "id\tfather\tmother" << "\n";
      print_samples_flag = true;
    }
    for (const auto& i: sampled_ids) {
      if (debug) children[i].print_parents_id();
      writing_sample << children[i] << "\n";
    }
  }
  std::vector<Individual> remove_migrant_fathers(const double migration_rate) {
    std::vector<Individual> migrant_fathers;
    std::vector<Individual> next_fathers;
    std::bernoulli_distribution bernoulli_migration(migration_rate);
    if (debug) std::cout << "before: fathers.size() =  " << fathers.size() << std::endl;
    for (size_t i = 0; i < fathers.size(); ++i) {
      //std::cout << "rng() = " << rng() << std::endl;
      //std::cout << "bernoulli = " << bernoulli_migration(rng) << std::endl;
      if (bernoulli_migration(rng)) {
        migrant_fathers.push_back(fathers[i]);
        if (debug) std::cout << "migrant_father_id: " << fathers[i].get_id() << std::endl;
      } else {
        next_fathers.push_back(fathers[i]);
      }
    }
    fathers.swap(next_fathers);
    next_fathers.clear();
    if (debug) std::cout << "remained: fathers.size() =  " << fathers.size() << std::endl;
    return migrant_fathers;
  }
  std::vector<Individual> remove_migrant_mothers(const double migration_rate) {
    std::vector<Individual> migrant_mothers;
    std::vector<Individual> next_mothers;
    std::bernoulli_distribution bernoulli_migration(migration_rate);
    if (debug) std::cout << "before: mothers.size() =  " << mothers.size() << std::endl;
    for (size_t i = 0; i < mothers.size(); ++i) {
      if (bernoulli_migration(rng)) {
        migrant_mothers.push_back(mothers[i]);
        if (debug) std::cout << "migrant_mother_id: " << mothers[i].get_id() << std::endl;
      } else {
        next_mothers.push_back(mothers[i]);
      }
    }
    mothers.swap(next_mothers);
    next_mothers.clear();
    if (debug) std::cout << "remained: mothers.size() =  " << mothers.size() << std::endl;
    return migrant_mothers;
  }
  void add_migrant_fathers(std::vector<Individual> migrant_fathers) {
    for (size_t i = 0; i < migrant_fathers.size(); ++i) {
      fathers.push_back(migrant_fathers[i]);
    }
  }
  void add_migrant_mothers(std::vector<Individual> migrant_mothers) {
    for (size_t i = 0; i < migrant_mothers.size(); ++i) {
      mothers.push_back(migrant_mothers[i]);
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
  if ( argc != 6 ) {
    fprintf(stderr,"Command line arguments are incorrect\n");
    return 0;
  }
  const size_t init_parent_number = atoi(argv[1]); //1
  const size_t sampled_number = atoi(argv[2]); //2
  const size_t migrant_number = atof(argv[3]); //3
  const double lambda_mean_0 = atof(argv[4]);//4
  const double lambda_mean_1 = atof(argv[5]);//5
  if ( migrant_numer > init_parent_number ) {
    fprintf(stderr,"migrant_number must be smaller than init_parent_number\n");
    return 0;
  }
  if (debug) std::cout << "INPUT: parent_pair_size = " << init_parent_number << ", sampled_number = " << sampled_number << ", migrant_number = " << migrant_number << ", lambda_mean_0 = " << lambda_mean_0 << ", and lambda_mean_1 = " << lambda_mean_1 << std::endl;

  Population pop0(init_parent_number);
  pop0.reproduction(lambda_mean_0);
  pop0.sampling(sampled_number, 0);

  Population pop1(init_parent_number - migrant_number);
  pop1.migration(pop0, migrant_number);
  pop1.reproduction(lambda_mean_1);
  pop1.sampling(sampled_number, 1);

  //pop1.print_family_id();

  if (debug) std::cerr << "-----------------------" << std::endl;

  end = std::chrono::system_clock::now();
  double elapsed_time = std::chrono::duration_cast<std::chrono::microseconds>(end-start).count();
  if (debug) std::cerr << "elapsed_time = " << elapsed_time/1000/1000 << " seconds" << std::endl;
}
