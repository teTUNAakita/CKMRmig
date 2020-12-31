/*
make
./a.out init_parent_pair_number sampled_number
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

class Individual
{
private:
  static int LATEST_ID;
  const int id;
  const std::pair<int, int> parents_ids;
public:
  Individual() : id(LATEST_ID++) {
  };
  Individual(const int father_id, const int mother_id) : id(LATEST_ID++), parents_ids(father_id, mother_id) {
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

std::random_device rd;
const auto seed = rd();
//std::mt19937 rng(seed);
std::mt19937 rng(1235);
//const double lambda = 3.0;
//const double migration_rate = 0.5;
//std::poisson_distribution<size_t> poisson(lambda);
std::bernoulli_distribution bernoulli50(0.5);
//std::bernoulli_distribution bernoulli_migration(migration_rate);

class Population
{
private:
  std::vector<Individual> fathers;
  std::vector<Individual> mothers;
  std::vector<Individual> children;
  bool debug = false;
  bool print_samples_flag = false;
public:
  Population(const size_t init_parent_number) :
  fathers(init_parent_number), mothers(init_parent_number) {
  };
  Population(std::vector<size_t> migratnt_indices) :
  fathers(migratnt_indices.size()) {
  };
  void reproduction(const double lambda) {
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
  void print_family_size() {
    if (debug) std::cerr << "fathers.size() = " << fathers.size() << ", mothers.size() = " << mothers.size() << ", children.size() = " << children.size() << std::endl;
  }
  bool check_size() {
    return fathers.empty() || mothers.empty();
  }
  void sampling(const size_t sampled_number, size_t rep) {
    std::vector<size_t> sampled_ids;
    std::vector<size_t> all_indices(children.size());
    std::iota(all_indices.begin(), all_indices.end(), 0);
    std::sample(all_indices.begin(), all_indices.end(), std::back_inserter(sampled_ids), sampled_number, rng);
    std::cout << "sampled_child & its parents_id: " << std::endl;
    std::string filename = "sample.txt";
    filename = std::to_string(rep) + filename;
    std::ofstream writing_sample;
    if (print_samples_flag) {
        writing_sample.open(filename, std::ios::app);
    } else {
        writing_sample.open(filename, std::ios::out);
        print_samples_flag = true;
    }
    for (const auto& i: sampled_ids) {
      children[i].print_parents_id();
      writing_sample << children[i] << "\n";
    }
  }
  std::vector<Individual> remove_migrant_fathers(const double migration_rate) {
    std::vector<Individual> migrant_fathers;
    std::vector<Individual> next_fathers;
    std::bernoulli_distribution bernoulli_migration(migration_rate);
    if (debug) std::cout << "before: fathers.size() =  " << fathers.size() << std::endl;
    for (size_t i = 0; i < fathers.size(); ++i) {
      if (bernoulli_migration(rng)) {
        next_fathers.push_back(fathers[i]);
      } else {
        migrant_fathers.push_back(fathers[i]);
        if (debug) std::cout << "migrant_id: " << fathers[i].get_id() << std::endl;
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
        next_mothers.push_back(mothers[i]);
      } else {
        migrant_mothers.push_back(mothers[i]);
        if (debug) std::cout << "migrant_id: " << mothers[i].get_id() << std::endl;
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
};

int Individual::LATEST_ID = 0;

int main(int argc, char *argv[])
{
  std::chrono::system_clock::time_point start, end;
  start = std::chrono::system_clock::now();
  if ( argc != 5 ) {
    fprintf(stderr,"Command line arguments are incorrect\n");
    return 0;
  }
  const size_t init_parent_number = atoi(argv[1]); //1
  const size_t sampled_number = atoi(argv[2]); //2
  const double migration_rate = atoi(argv[3]); //3
  const double lambda = atoi(argv[4]);//4

  Population pop0(init_parent_number);
  pop0.reproduction(lambda);
  pop0.sampling(sampled_number, 0);

  Population pop1(init_parent_number);
  pop1.reproduction(lambda);

  std::vector<Individual> tmp_migrant_01_fathers = pop0.remove_migrant_fathers(migration_rate);
  std::vector<Individual> tmp_migrant_01_mothers = pop0.remove_migrant_mothers(migration_rate);
  std::vector<Individual> tmp_migrant_10_fathers = pop1.remove_migrant_fathers(migration_rate);
  std::vector<Individual> tmp_migrant_10_mothers = pop1.remove_migrant_mothers(migration_rate);

  pop0.add_migrant_fathers(tmp_migrant_10_fathers);
  pop0.add_migrant_mothers(tmp_migrant_10_mothers);
  pop0.print_family_size();
  pop1.add_migrant_fathers(tmp_migrant_01_fathers);
  pop1.add_migrant_mothers(tmp_migrant_01_mothers);
  pop1.print_family_size();

  pop0.reproduction(lambda);
  pop1.reproduction(lambda);
  pop1.sampling(sampled_number, 1);


  std::cerr << "-----------------------" << std::endl;

  end = std::chrono::system_clock::now();
  double elapsed_time = std::chrono::duration_cast<std::chrono::microseconds>(end-start).count();
  std::cerr << "elapsed_time = " << elapsed_time << std::endl;
}
