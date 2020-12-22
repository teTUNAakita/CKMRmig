/*
２つの集団を定義して，それぞれでIDを生成する
親の数を与えて、１つの集団で子供作る！
・移動なし
・雌雄あり
・メスがポアソンで残す、父はまずはランダム
Age structure #15-3
ランダム交配で子孫を残す。
メスがポアソンで子を残し、その子達の父親はランダムに決まる
齢に関係なく、個体は死亡率50%で死ぬ
死んだ個体の祖先関係も出力する
*/

#include <iostream>
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
  //unsigned int location;
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
};

std::random_device rd;
const auto seed = rd();
//std::mt19937 rng(seed);
std::mt19937 rng(1235);
const double lambda = 3.0;
const double migration_rate = 0.5;
std::poisson_distribution<size_t> poisson(lambda);
std::bernoulli_distribution bernoulli(0.5);
std::bernoulli_distribution bernoulli_migration(migration_rate);

class Population
{
private:
  std::vector<Individual> fathers;
  std::vector<Individual> mothers;
  std::vector<Individual> children;
  std::vector<size_t> migrant_indices;
  std::vector<Individual> migrant_fathers;
  const size_t sampled_number = 2;
  bool debug = true; //false;
public:
  Population(size_t init_parent_number) :
  fathers(init_parent_number), mothers(init_parent_number) {
  };
  Population(std::vector<size_t> migratnt_indices) :
  fathers(migratnt_indices.size()) {
  };
  void reproduction() {
    const size_t father_number = fathers.size();
    const size_t mother_number = mothers.size();
    std::uniform_int_distribution<size_t> uniform_int(0, father_number - 1);
    if (debug) std::cerr << "fathers.size() = " << fathers.size() << ", mothers.size() = " << mothers.size() << std::endl;
    for (size_t i = 0; i < mother_number; ++i) {
      const size_t child_number = poisson(rng);
      if (debug) std::cerr << "child_number = " << child_number << std::endl;
      for (size_t j = 0; j < child_number; ++j) {
        const size_t father_index = uniform_int(rng);
        children.push_back( Individual( fathers[father_index].get_id(), mothers[i].get_id() ) );
        children.back().print_parents_id();
      }
    }
  }
  void print_family_size() {
    if (debug) std::cerr << "fathers.size() = " << fathers.size() << ", mothers.size() = " << mothers.size() << ", children.size() = " << children.size() << std::endl;
  }
  void print_family_ids() const {
    for (size_t i = 0; i < fathers.size(); i++) {
      std::cerr << "father_id: " << (fathers[i]).get_id() << std::endl;
    }
    for (size_t i = 0; i < mothers.size(); i++) {
      std::cerr << "mother_id: " << (mothers[i]).get_id() << std::endl;
    }
  }
  bool check_size() {
    return fathers.empty() || mothers.empty();
  }
  void print_samples() {
    print_samples(children, sampled_number);
  }
  void print_samples(const std::vector<Individual>& individuals, const size_t sampled_number) {
    std::vector<int> sampled_indices;
    std::vector<int> all_indices(individuals.size());
    std::iota(all_indices.begin(), all_indices.end(), 0);
    std::sample(all_indices.begin(), all_indices.end(), std::back_inserter(sampled_indices), sampled_number, rng);
    std::cout << "sampled_child_id: " << std::endl;
    for (const auto& i: sampled_indices) {
      std::cout << individuals[i].get_id() << std::endl;
    }
  }
  std::vector<Individual> remove_migrant_fathers() {
    std::vector<Individual> next_fathers;
    std::cout << "before: fathers.size() =  " << fathers.size() << std::endl;
    for (size_t i = 0; i < fathers.size(); ++i) {
      if (bernoulli_migration(rng)) {
        next_fathers.push_back(fathers[i]);
      } else {
        migrant_fathers.push_back(fathers[i]);
        std::cout << "migrant_id: " << fathers[i].get_id() << std::endl;
      }
    }
    fathers.swap(next_fathers);
    next_fathers.clear();
    std::cout << "remained: fathers.size() =  " << fathers.size() << std::endl;
    return migrant_fathers;
  }
  void add_migrant_fathers(std::vector<Individual> migrant_fathers) {
    for (size_t i = 0; i < migrant_fathers.size(); ++i) {
      fathers.push_back(migrant_fathers[i]);
    }
    //fathers.swap(next_fathers);
    //next_fathers.clear();
  }
  /*
  std::vector<size_t> get_migrant_indices() const {
    return migrant_indices;
  }
  std::vector<Individual> get_migratnt() const {
    return migrant_fathers;
  }*/
};

//tmp_fathers.push_back(pop1.fathers);
// select imigrant ID
/*
for (size_t i = 0; i < pop0.fathers.size(); ++i) {
if (bernoulli_migration(rng)) {
next_fathers.push_back(fathers[i]);
} else {
migrant_fathers.push_back(fathers[i]);
std::cout << "migrant_id: " << fathers[i].get_id() << std::endl;
migrant_indices.push_back(fathers[i].get_id());
}
}
fathers.swap(next_fathers);

pop0.print_family_size();
pop0.remove_migrants();
size_t migrant_number = (pop0.get_migratnt_indices()).size();
for (size_t i = 0; i < migrant_number; ++i) {
std::cout << "tmp_migrants_id: " << pop0.get_migratnt_indices()[i] << std::endl;
}
pop0.print_family_size();
return pop0.get_migratnt_indices();
*/


int Individual::LATEST_ID = 0;

int main()
{
  std::chrono::system_clock::time_point start, end;
  start = std::chrono::system_clock::now();

  size_t init_parent_number = 3;

  Population pop0(init_parent_number);
  pop0.reproduction();
  //pop0.print_samples();

  Population pop1(init_parent_number);
  pop1.reproduction();

  std::vector<Individual> tmp_migrant_01_fathers = pop0.remove_migrant_fathers();
  std::vector<Individual> tmp_migrant_10_fathers = pop1.remove_migrant_fathers();

  pop0.add_migrant_fathers(tmp_migrant_10_fathers);
  pop0.print_family_size();
  pop1.add_migrant_fathers(tmp_migrant_01_fathers);
  pop1.print_family_size();
    //std::vector<size_t> migratnt_indices = migration(pop0);

  //Population pop12(migratnt_indices);
  //pop12.print_family_ids();

  std::cerr << "-----------------------" << std::endl;
  pop0.print_family_ids();
  std::cerr << "-----------------------" << std::endl;
  pop1.print_family_ids();
  //pop1.print_samples();

  //migration(pop0, pop1);

  end = std::chrono::system_clock::now();
  double elapsed_time = std::chrono::duration_cast<std::chrono::microseconds>(end-start).count();
  std::cerr << "elapsed_time = " << elapsed_time << std::endl;
}
