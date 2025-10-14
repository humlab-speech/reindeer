// Rcpp implementations for performance-critical emuR functions
// Compile with: Rcpp::sourceCpp("emur_optimized.cpp")

#include <Rcpp.h>
#include <unordered_set>
#include <unordered_map>
#include <vector>
#include <string>
#include <algorithm>

using namespace Rcpp;

// [[Rcpp::export]]
StringVector get_allAttributeNames_cpp(List levelDefinitions) {
  std::unordered_set<std::string> uniqueNames;
  
  for (int i = 0; i < levelDefinitions.size(); i++) {
    List lvlDef = levelDefinitions[i];
    List attrDefs = lvlDef["attributeDefinitions"];
    
    for (int j = 0; j < attrDefs.size(); j++) {
      List attrDef = attrDefs[j];
      std::string name = as<std::string>(attrDef["name"]);
      uniqueNames.insert(name);
    }
  }
  
  StringVector result(uniqueNames.size());
  int idx = 0;
  for (const auto& name : uniqueNames) {
    result[idx++] = name;
  }
  
  return result;
}

// [[Rcpp::export]]
CharacterVector get_linkLevelChildrenNames_cpp(List linkDefinitions, 
                                                std::string superlevelName) {
  std::vector<std::string> children;
  
  for (int i = 0; i < linkDefinitions.size(); i++) {
    List linkDef = linkDefinitions[i];
    std::string superName = as<std::string>(linkDef["superlevelName"]);
    
    if (superName == superlevelName) {
      std::string subName = as<std::string>(linkDef["sublevelName"]);
      children.push_back(subName);
    }
  }
  
  return wrap(children);
}

// [[Rcpp::export]]
List expand_linkPath_cpp(StringVector path) {
  int pathLen = path.size();
  if (pathLen == 1) {
    return List::create();
  }
  
  List result(pathLen - 1);
  for (int i = 0; i < pathLen - 1; i++) {
    StringVector subPath(pathLen - i);
    for (int j = 0; j < pathLen - i; j++) {
      subPath[j] = path[j];
    }
    result[i] = subPath;
  }
  
  return result;
}

// Memoized recursive path building
class PathBuilder {
private:
  std::unordered_map<std::string, List> memo;
  List linkDefinitions;
  
  StringVector getChildren(const std::string& levelName) {
    std::vector<std::string> children;
    
    for (int i = 0; i < linkDefinitions.size(); i++) {
      List linkDef = linkDefinitions[i];
      std::string superName = as<std::string>(linkDef["superlevelName"]);
      
      if (superName == levelName) {
        std::string subName = as<std::string>(linkDef["sublevelName"]);
        children.push_back(subName);
      }
    }
    
    return wrap(children);
  }
  
public:
  PathBuilder(List links) : linkDefinitions(links) {}
  
  List buildPaths(const std::string& levelName) {
    // Check memo
    auto it = memo.find(levelName);
    if (it != memo.end()) {
      return it->second;
    }
    
    List paths;
    StringVector children = getChildren(levelName);
    
    if (children.size() == 0) {
      paths = List::create(StringVector::create(levelName));
    } else {
      std::vector<StringVector> allPaths;
      
      for (int i = 0; i < children.size(); i++) {
        List childPaths = buildPaths(as<std::string>(children[i]));
        
        for (int j = 0; j < childPaths.size(); j++) {
          StringVector childPath = childPaths[j];
          StringVector fullPath(childPath.size() + 1);
          fullPath[0] = levelName;
          
          for (int k = 0; k < childPath.size(); k++) {
            fullPath[k + 1] = childPath[k];
          }
          
          allPaths.push_back(fullPath);
        }
      }
      
      paths = wrap(allPaths);
    }
    
    // Memoize
    memo[levelName] = paths;
    return paths;
  }
};

// [[Rcpp::export]]
List build_sublevelPathes_cpp(List DBconfig, std::string levelName) {
  List linkDefs = DBconfig["linkDefinitions"];
  PathBuilder builder(linkDefs);
  return builder.buildPaths(levelName);
}

// [[Rcpp::export]]
List build_allHierarchyPaths_cpp(List schema) {
  List levelDefs = schema["levelDefinitions"];
  List linkDefs = schema["linkDefinitions"];
  
  std::unordered_set<std::string> uniquePaths;
  std::vector<StringVector> resultPaths;
  
  PathBuilder builder(linkDefs);
  
  for (int i = 0; i < levelDefs.size(); i++) {
    List lvlDef = levelDefs[i];
    std::string levelName = as<std::string>(lvlDef["name"]);
    
    List paths = builder.buildPaths(levelName);
    
    for (int j = 0; j < paths.size(); j++) {
      StringVector path = paths[j];
      List expanded = expand_linkPath_cpp(path);
      
      // Add original path
      std::string pathKey = "";
      for (int k = 0; k < path.size(); k++) {
        if (k > 0) pathKey += "|";
        pathKey += as<std::string>(path[k]);
      }
      
      if (uniquePaths.find(pathKey) == uniquePaths.end()) {
        uniquePaths.insert(pathKey);
        resultPaths.push_back(path);
      }
      
      // Add expanded paths
      for (int k = 0; k < expanded.size(); k++) {
        StringVector expPath = expanded[k];
        pathKey = "";
        for (int l = 0; l < expPath.size(); l++) {
          if (l > 0) pathKey += "|";
          pathKey += as<std::string>(expPath[l]);
        }
        
        if (uniquePaths.find(pathKey) == uniquePaths.end()) {
          uniquePaths.insert(pathKey);
          resultPaths.push_back(expPath);
        }
      }
    }
  }
  
  return wrap(resultPaths);
}

// [[Rcpp::export]]
StringVector find_segmentLevels_cpp(List DBconfig, std::string attrName) {
  List levelDefs = DBconfig["levelDefinitions"];
  std::unordered_set<std::string> segmentLevels;
  
  // Find level for attribute
  std::string levelName;
  bool found = false;
  
  for (int i = 0; i < levelDefs.size() && !found; i++) {
    List lvlDef = levelDefs[i];
    List attrDefs = lvlDef["attributeDefinitions"];
    
    for (int j = 0; j < attrDefs.size(); j++) {
      List attrDef = attrDefs[j];
      if (as<std::string>(attrDef["name"]) == attrName) {
        levelName = as<std::string>(lvlDef["name"]);
        found = true;
        break;
      }
    }
  }
  
  if (!found) {
    return StringVector();
  }
  
  // Build extended link definitions and find segment levels
  PathBuilder builder(DBconfig["linkDefinitions"]);
  List paths = builder.buildPaths(levelName);
  
  for (int i = 0; i < paths.size(); i++) {
    StringVector path = paths[i];
    
    for (int j = 1; j < path.size(); j++) {
      std::string targetLevel = as<std::string>(path[j]);
      
      // Find level definition and check if it's SEGMENT type
      for (int k = 0; k < levelDefs.size(); k++) {
        List lvlDef = levelDefs[k];
        if (as<std::string>(lvlDef["name"]) == targetLevel) {
          if (as<std::string>(lvlDef["type"]) == "SEGMENT") {
            segmentLevels.insert(targetLevel);
          }
          break;
        }
      }
    }
  }
  
  StringVector result(segmentLevels.size());
  int idx = 0;
  for (const auto& level : segmentLevels) {
    result[idx++] = level;
  }
  
  return result;
}

// Fast level definition lookup using hash map
// [[Rcpp::export]]
List get_levelDefinition_cpp(List DBconfig, std::string name) {
  List levelDefs = DBconfig["levelDefinitions"];
  
  for (int i = 0; i < levelDefs.size(); i++) {
    List lvlDef = levelDefs[i];
    if (as<std::string>(lvlDef["name"]) == name) {
      return lvlDef;
    }
  }
  
  return R_NilValue;
}

// Optimized connectivity check for hierarchy paths
// [[Rcpp::export]]
List get_hierPathsConnectingLevels_cpp(List DBconfig, 
                                        std::string levelName1, 
                                        std::string levelName2) {
  List allPaths = build_allHierarchyPaths_cpp(DBconfig);
  std::vector<StringVector> connectingPaths;
  
  for (int i = 0; i < allPaths.size(); i++) {
    StringVector path = allPaths[i];
    if (path.size() < 1) continue;
    
    std::string first = as<std::string>(path[0]);
    std::string last = as<std::string>(path[path.size() - 1]);
    
    if ((first == levelName1 && last == levelName2) ||
        (first == levelName2 && last == levelName1)) {
      connectingPaths.push_back(path);
    }
  }
  
  return wrap(connectingPaths);
}
