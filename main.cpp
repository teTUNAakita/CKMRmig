/*
make
./a.out init_parent_pair_number sampled_number migration_rate lambda_1 lambda_2
./a.out 100 20 0.2 3 10
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
  const int lambda;
  const std::pair<int, int> parents_ids;
  std::exponential_distribution<> exp_dist;
  const double lambda;
public:
  Individual(const double lambda_mean) :
  id(LATEST_ID++),
  exp_dist(1/lambda_mean),
  lambda(exp_dist(rng)) {
    //std::cout << lambda_mean  << "\t" << lambda << std::endl;
    //std::cout << parents_ids.first << "\t" << parents_ids.second << std::endl; // parents_ids = (0,0)
  };
  Individual(const int father_id, const int mother_id) :
  id(LATEST_ID++),
  parents_ids(father_id, mother_id),
  lambda(0) {
    //std::cout << lambda << std::endl;
  };
  int get_id() const {
    return id;
  }
  double get_lambda() const {
    return lambda;
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
  Population(const size_t init_parent_number, const double lambda_mean) {
    for(size_t i = 0; i < init_parent_number; i++) {
      fathers.push_back(Individual(lambda_mean));
      mothers.push_back(Individual(lambda_mean));
    }
  }
  size_t roulette_selection(const std::vector<double> cumsum_weight) const {
      std::uniform_real_distribution<> uniform_real(0, cumsum_weight.back());
      double r = uniform_real(rng);
      auto it = std::upper_bound(cumsum_weight.begin(), cumsum_weight.end(), r);
      return (it - cumsum_weight.begin());
  }
  void reproduction() {
    const size_t father_number = fathers.size();
    const size_t mother_number = mothers.size();
    std::vector<double> fathers_lambda(father_number);
    std::uniform_int_distribution<size_t> uniform_int(0, father_number - 1);
    //std::poisson_distribution<size_t> poisson(lambda);
    if (debug) std::cerr << "fathers.size() = " << fathers.size() << ", mothers.size() = " << mothers.size() << std::endl;
    for (size_t i = 0; i < mother_number; ++i) {
      std::poisson_distribution<> poi_dist(mothers[i].get_lambda());
      const size_t child_number = poi_dist(rng);
      //std::cout << mothers[i].get_lambda() << std::endl;
      if (debug) std::cerr << "child_number = " << child_number << std::endl;
      for (size_t i = 0; i < father_number; ++i) {
        fathers_lambda[i] = fathers[i].get_lambda();
      }
      const std::vector<double> cumsum_fathers_lambda = cumsum(fathers_lambda);
      for (size_t i = 0; i < fathers.size(); ++i) {
        if (debug) std::cout << "cumsum_fathers_lambda[i] = " << cumsum_fathers_lambda[i] << std::endl;
      }
      for (size_t i = 0; i < child_number; ++i) {
        std::uniform_real_distribution<> uniform_real(0, cumsum_fathers_lambda.back());
        double r = uniform_real(rng);
        if (debug) std::cout << "r = " << r << std::endl;
        size_t father_index = 0;
        while (father_index < cumsum_fathers_lambda.size()) {
          if(r < cumsum_fathers_lambda[father_index]) {
            if (debug) std::cout << "father_id: " << father_index << std::endl;
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
};

int Individual::LATEST_ID = 0;

int main(int argc, char *argv[])
{
  // std::chrono::system_clock::time_point start, end;
  // start = std::chrono::system_clock::now();
  if ( argc != 6 ) {
    fprintf(stderr,"Command line arguments are incorrect\n");
    return 0;
  }
  const size_t init_parent_number = atoi(argv[1]); //1
  const size_t sampled_number = atoi(argv[2]); //2
  const double migration_rate = atof(argv[3]); //3
  const double lambda_mean_0 = atof(argv[4]);//4
  const double lambda_mean_1 = atof(argv[5]);//5

  std::cout << "INPUT: parent_pair_size = " << init_parent_number << ", sampled_number = " << sampled_number << ", migration_rate = " << migration_rate << ", lambda_mean_0 = " << lambda_mean_0 << ", and lambda_mean_1 = " << lambda_mean_1 << std::endl;

  Population pop0(init_parent_number, lambda_mean_0);
  pop0.reproduction();
  pop0.sampling(sampled_number, 0);

  Population pop1(init_parent_number, lambda_mean_1);
  ////pop1.reproduction(lambda_1); // must exclude these offspring from sampling

  std::vector<Individual> tmp_migrant_01_fathers = pop0.remove_migrant_fathers(migration_rate);
  std::vector<Individual> tmp_migrant_01_mothers = pop0.remove_migrant_mothers(migration_rate);
  std::vector<Individual> tmp_migrant_10_fathers = pop1.remove_migrant_fathers(migration_rate);
  std::vector<Individual> tmp_migrant_10_mothers = pop1.remove_migrant_mothers(migration_rate);

  //std::cout << "tmp_migrant_01_fathers.size() = " << tmp_migrant_01_fathers.size() << std::endl;
  //std::cout << "tmp_migrant_01_mothers.size() = " << tmp_migrant_01_mothers.size() << std::endl;
  //std::cout << "tmp_migrant_10_fathers.size() = " << tmp_migrant_10_fathers.size() << std::endl;
  //std::cout << "tmp_migrant_10_mothers.size() = " << tmp_migrant_10_mothers.size() << std::endl;

  pop0.add_migrant_fathers(tmp_migrant_10_fathers);
  pop0.add_migrant_mothers(tmp_migrant_10_mothers);
  //pop0.print_family_size();
  pop1.add_migrant_fathers(tmp_migrant_01_fathers);
  pop1.add_migrant_mothers(tmp_migrant_01_mothers);
  //pop1.print_family_size();

  ////pop0.reproduction(lambda_0);
  pop1.reproduction();
  pop1.sampling(sampled_number, 1);

  //pop1.print_family_id();

  // std::cerr << "-----------------------" << std::endl;

  //std::bernoulli_distribution dist(0.5);
  //std::cout << dist(rng) << std::endl;
  //std::cout << "migration_rate = " << migration_rate << std::endl;

  // end = std::chrono::system_clock::now();
  // double elapsed_time = std::chrono::duration_cast<std::chrono::microseconds>(end-start).count();
  // std::cerr << "elapsed_time = " << elapsed_time/1000/1000 << " seconds" << std::endl;
}
