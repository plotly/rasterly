#include <cmath>
#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
List agg_sumCpp(ListOf<NumericMatrix> L,
                CharacterVector levels, CharacterVector category,
                int& plot_width, int& plot_height,
                NumericVector& x_range, NumericVector& y_range,
                NumericVector& xlim, NumericVector& ylim,
                NumericVector& x, NumericVector& y, NumericVector& on,
                NumericVector& size, CharacterVector& glyph) {
  int n = x.size();
  int id_x = 0;
  int id_y = 0;
  
  int levels_size = levels.size();
  // Each element in the list L has the same size Matrix
  int id = 0;
  
  double xmin = x_range[0];
  double xmax = x_range[1];
  double ymin = y_range[0];
  double ymax = y_range[1];
  
  bool is_on = true;
  if(on.size() == 0) is_on = false;
  bool is_size = true;
  if(size.size() == 0) is_size = false;
  
  // remove unnecessary loop by logical check
  if(is_size) {
    
    for(int i=0; i < n; i++) {
      
      for(int l = 0; l < levels_size; l++) {
        if(category[i] == levels[l]) {
          id = l;
          break;
        }
      }
      
      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));
        
        if(glyph[0] == "square") {
          
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            for(int k = id_x - size[i]; k < id_x + size[i] + 1; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                if(is_on) {
                  L[id](j, k) += on[i];
                } else {
                  L[id](j, k) += 1;
                }
              }
            }
          }
        } else if(glyph[0] == "circle") {
          
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            
            int m = abs(id_y - j);
            
            for(int k = id_x - size[i] + m; k < id_x + size[i] + 1 - m; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                
                if(is_on) {
                  L[id](j, k) += on[i];
                } else {
                  L[id](j, k) += 1;
                }
              }
            }
          }
        }
      }
    }
    
  } else {
    
    for(int i=0; i < n; i++) {
      
      for(int l = 0; l < levels_size; l++) {
        if(category[i] == levels[l]) {
          id = l;
          break;
        }
      }
      
      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));
        if(is_on) {
          L[id](id_y, id_x) += on[i];
        } else {
          L[id](id_y, id_x) += 1;
        }
      }
    }
  }
  return(L);
}

//[[Rcpp::export]]
List agg_anyCpp(ListOf<NumericMatrix> L,
                CharacterVector levels, CharacterVector category,
                int& plot_width, int& plot_height,
                NumericVector& x_range, NumericVector& y_range,
                NumericVector& xlim, NumericVector& ylim,
                NumericVector& x, NumericVector& y, NumericVector& on,
                NumericVector& size, CharacterVector& glyph) {
  int n = x.size();
  int id_x = 0;
  int id_y = 0;
  
  int levels_size = levels.size();
  // Each element in the list L has the same size Matrix
  int id = 0;
  
  double xmin = x_range[0];
  double xmax = x_range[1];
  double ymin = y_range[0];
  double ymax = y_range[1];
  
  bool is_on = true;
  if(on.size() == 0) is_on = false;
  bool is_size = true;
  if(size.size() == 0) is_size = false;
  
  // remove unnecessary loop by logical check
  if(is_size) {
    
    for(int i=0; i < n; i++) {
      
      for(int l = 0; l < levels_size; l++) {
        if(category[i] == levels[l]) {
          id = l;
          break;
        }
      }
      
      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));
        
        if(glyph[0] == "square") {
          
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            for(int k = id_x - size[i]; k < id_x + size[i] + 1; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                if(is_on) {
                  if(on[i] != 0) {
                    L[id](j, k) = 1;
                  }
                } else {
                  L[id](j, k) = 1;
                }
              }
            }
          }
        } else if(glyph[0] == "circle") {
          
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            
            int m = abs(id_y - j);
            
            for(int k = id_x - size[i] + m; k < id_x + size[i] + 1 - m; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                
                if(is_on) {
                  if(on[i] != 0) {
                    L[id](j, k) = 1;
                  }
                } else {
                  L[id](j, k) = 1;
                }
              }
            }
          }
        }
      }
    }
    
  } else {
    
    for(int i=0; i < n; i++) {
      
      for(int l = 0; l < levels_size; l++) {
        if(category[i] == levels[l]) {
          id = l;
          break;
        }
      }
      
      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));
        if(is_on) {
          if(on[i] != 0) {
            L[id](id_y, id_x) = 1;
          }
        } else {
          L[id](id_y, id_x) = 1;
        }
      }
    }
  }
  return(L);
}

//[[Rcpp::export]]
List agg_meanCpp(ListOf<NumericMatrix> L,
                 CharacterVector levels, CharacterVector category,
                 int& plot_width, int& plot_height,
                 NumericVector& x_range, NumericVector& y_range,
                 NumericVector& xlim, NumericVector& ylim,
                 NumericVector& x, NumericVector& y, NumericVector& on,
                 NumericVector& size, CharacterVector& glyph) {
  int n = x.size();
  int id_x = 0;
  int id_y = 0;
  
  int levels_size = levels.size();
  // Each element in the list L has the same size Matrix
  int id = 0;
  
  double xmin = x_range[0];
  double xmax = x_range[1];
  double ymin = y_range[0];
  double ymax = y_range[1];
  
  bool is_size = true;
  if(size.size() == 0) is_size = false;
  
  // remove unnecessary loop by logical check
  if(is_size) {
    
    for(int i=0; i < n; i++) {
      
      for(int l = 0; l < levels_size; l++) {
        if(category[i] == levels[l]) {
          id = l;
          break;
        }
      }
      
      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));
        
        if(glyph[0] == "square") {
          
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            for(int k = id_x - size[i]; k < id_x + size[i] + 1; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                L[id](j, k) = on[i];
              }
            }
          }
        } else if(glyph[0] == "circle") {
          
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            
            int m = abs(id_y - j);
            
            for(int k = id_x - size[i] + m; k < id_x + size[i] + 1 - m; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                
                L[id](j, k) = on[i];
              }
            }
          }
        }
      }
    }
    
  } else {
    
    for(int i=0; i < n; i++) {
      
      for(int l = 0; l < levels_size; l++) {
        if(category[i] == levels[l]) {
          id = l;
          break;
        }
      }
      
      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));
        L[id](id_y, id_x) = on[i];
      }
    }
  }
  return(L);
}

//[[Rcpp::export]]
List agg_firstCpp(ListOf<NumericMatrix> L,
                CharacterVector levels, CharacterVector category,
                int& plot_width, int& plot_height,
                NumericVector& x_range, NumericVector& y_range,
                NumericVector& xlim, NumericVector& ylim,
                NumericVector& x, NumericVector& y, NumericVector& on,
                NumericVector& size, CharacterVector& glyph) {
  int n = x.size();
  int id_x = 0;
  int id_y = 0;
  
  int levels_size = levels.size();
  // Each element in the list L has the same size Matrix
  int id = 0;
  
  double xmin = x_range[0];
  double xmax = x_range[1];
  double ymin = y_range[0];
  double ymax = y_range[1];
  
  bool is_size = true;
  if(size.size() == 0) is_size = false;
  
  // remove unnecessary loop by logical check
  if(is_size) {
    
    for(int i=(n-1); i >= 0; i--) {
      
      for(int l = 0; l < levels_size; l++) {
        if(category[i] == levels[l]) {
          id = l;
          break;
        }
      }
      
      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));
        
        if(glyph[0] == "square") {
          
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            for(int k = id_x - size[i]; k < id_x + size[i] + 1; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                L[id](j, k) = on[i];
              }
            }
          }
        } else if(glyph[0] == "circle") {
          
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            
            int m = abs(id_y - j);
            
            for(int k = id_x - size[i] + m; k < id_x + size[i] + 1 - m; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                
                L[id](j, k) = on[i];
              }
            }
          }
        }
      }
    }
    
  } else {
    
    for(int i=(n-1); i >= 0; i--) {
      
      for(int l = 0; l < levels_size; l++) {
        if(category[i] == levels[l]) {
          id = l;
          break;
        }
      }
      
      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));
        L[id](id_y, id_x) = on[i];
      }
    }
  }
  return(L);
}

//[[Rcpp::export]]
List agg_lastCpp(ListOf<NumericMatrix> L,
                 CharacterVector levels, CharacterVector category,
                 int& plot_width, int& plot_height,
                 NumericVector& x_range, NumericVector& y_range,
                 NumericVector& xlim, NumericVector& ylim,
                 NumericVector& x, NumericVector& y, NumericVector& on,
                 NumericVector& size, CharacterVector& glyph) {
  int n = x.size();
  int id_x = 0;
  int id_y = 0;
  
  int levels_size = levels.size();
  // Each element in the list L has the same size Matrix
  int id = 0;
  
  double xmin = x_range[0];
  double xmax = x_range[1];
  double ymin = y_range[0];
  double ymax = y_range[1];
  
  bool is_size = true;
  if(size.size() == 0) is_size = false;
  
  // remove unnecessary loop by logical check
  if(is_size) {
    
    for(int i=0; i < n; i++) {
      
      for(int l = 0; l < levels_size; l++) {
        if(category[i] == levels[l]) {
          id = l;
          break;
        }
      }
      
      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));
        
        if(glyph[0] == "square") {
          
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            for(int k = id_x - size[i]; k < id_x + size[i] + 1; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                L[id](j, k) = on[i];
              }
            }
          }
        } else if(glyph[0] == "circle") {
          
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            
            int m = abs(id_y - j);
            
            for(int k = id_x - size[i] + m; k < id_x + size[i] + 1 - m; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                
                L[id](j, k) = on[i];
              }
            }
          }
        }
      }
    }
    
  } else {
    
    for(int i=0; i < n; i++) {
      
      for(int l = 0; l < levels_size; l++) {
        if(category[i] == levels[l]) {
          id = l;
          break;
        }
      }
      
      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));
        L[id](id_y, id_x) = on[i];
      }
    }
  }
  return(L);
}

//[[Rcpp::export]]
List agg_maxCpp(ListOf<NumericMatrix> L,
                CharacterVector levels, CharacterVector category,
                int& plot_width, int& plot_height,
                NumericVector& x_range, NumericVector& y_range,
                NumericVector& xlim, NumericVector& ylim,
                NumericVector& x, NumericVector& y, NumericVector& on,
                NumericVector& size, CharacterVector& glyph) {
  int n = x.size();
  int id_x = 0;
  int id_y = 0;
  
  int levels_size = levels.size();
  // Each element in the list L has the same size Matrix
  int id = 0;
  
  double xmin = x_range[0];
  double xmax = x_range[1];
  double ymin = y_range[0];
  double ymax = y_range[1];
  
  bool is_size = true;
  if(size.size() == 0) is_size = false;
  
  // make on is larger than zero;
  double min_on = min(on);
  on = on - min_on + 1e-8;
  
  // remove unnecessary loop by logical check
  if(is_size) {
    
    for(int i=0; i < n; i++) {
      
      for(int l = 0; l < levels_size; l++) {
        if(category[i] == levels[l]) {
          id = l;
          break;
        }
      }
      
      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));
        
        if(glyph[0] == "square") {
          
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            for(int k = id_x - size[i]; k < id_x + size[i] + 1; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                if(L[id](j, k) < on[i]) L[id](j, k) = on[i];
              }
            }
          }
        } else if(glyph[0] == "circle") {
          
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            
            int m = abs(id_y - j);
            
            for(int k = id_x - size[i] + m; k < id_x + size[i] + 1 - m; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                if(L[id](j, k) < on[i]) L[id](j, k) = on[i];
              }
            }
          }
        }
      }
    }
    
  } else {
    
    for(int i=0; i < n; i++) {
      
      for(int l = 0; l < levels_size; l++) {
        if(category[i] == levels[l]) {
          id = l;
          break;
        }
      }
      
      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));
        if(L[id](id_y, id_x) < on[i]) L[id](id_y, id_x) = on[i];
      }
    }
  }
  return(L);
}

//[[Rcpp::export]]
List agg_minCpp(ListOf<NumericMatrix> L,
                CharacterVector levels, CharacterVector category,
                int& plot_width, int& plot_height,
                NumericVector& x_range, NumericVector& y_range,
                NumericVector& xlim, NumericVector& ylim,
                NumericVector& x, NumericVector& y, NumericVector& on,
                NumericVector& size, CharacterVector& glyph) {
  int n = x.size();
  int id_x = 0;
  int id_y = 0;
  
  int levels_size = levels.size();
  // Each element in the list L has the same size Matrix
  int id = 0;
  
  double xmin = x_range[0];
  double xmax = x_range[1];
  double ymin = y_range[0];
  double ymax = y_range[1];
  
  bool is_size = true;
  if(size.size() == 0) is_size = false;
  
  // make on is smaller than zero;
  double max_on = max(on);
  on = on - max_on - 1e-8;
  
  // remove unnecessary loop by logical check
  if(is_size) {
    
    for(int i=0; i < n; i++) {
      
      for(int l = 0; l < levels_size; l++) {
        if(category[i] == levels[l]) {
          id = l;
          break;
        }
      }
      
      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));
        
        if(glyph[0] == "square") {
          
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            for(int k = id_x - size[i]; k < id_x + size[i] + 1; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                if(L[id](j, k) > on[i]) L[id](j, k) = on[i];
              }
            }
          }
        } else if(glyph[0] == "circle") {
          
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            
            int m = abs(id_y - j);
            
            for(int k = id_x - size[i] + m; k < id_x + size[i] + 1 - m; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                if(L[id](j, k) > on[i]) L[id](j, k) = on[i];
              }
            }
          }
        }
      }
    }
    
  } else {
    
    for(int i=0; i < n; i++) {
      
      for(int l = 0; l < levels_size; l++) {
        if(category[i] == levels[l]) {
          id = l;
          break;
        }
      }
      
      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));
        if(L[id](id_y, id_x) > on[i]) L[id](id_y, id_x) = on[i];
      }
    }
  }
  return(L);
}
