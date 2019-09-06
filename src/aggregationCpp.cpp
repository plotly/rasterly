
#include <cmath>
#include <Rcpp.h>
using namespace Rcpp;

// for new R-devel
void R_init_rasterizer(DllInfo* info) {
  R_registerRoutines(info, NULL, NULL, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}

//[[Rcpp::export]]
NumericMatrix aggregation_sumCpp(int& plot_width, int& plot_height,
                                 NumericVector& x_range, NumericVector& y_range,
                                 NumericVector& xlim, NumericVector& ylim,
                                 NumericVector& x, NumericVector& y, NumericVector& on,
                                 NumericVector& size, CharacterVector& glyph) {
  int n = x.size();
  int id_x = 0;
  int id_y = 0;

  NumericMatrix R(plot_height, plot_width);
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


      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {

        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));

        if(glyph[0] == "square") {

          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            for(int k = id_x - size[i]; k < id_x + size[i] + 1; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                if(is_on) {
                  R(j, k) += on[i];
                } else {
                  R(j, k) += 1;
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
                  R(j, k) += on[i];
                } else {
                  R(j, k) += 1;
                }
              }
            }
          }
        }
      }
    }

  } else {

    for(int i=0; i < n; i++) {

      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {

        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));
        if(is_on) {
          R(id_y, id_x) += on[i];
        } else {
          R(id_y, id_x) += 1;
        }
      }
    }
  }

  return(R);
}


//[[Rcpp::export]]
NumericMatrix aggregation_anyCpp(int& plot_width, int& plot_height,
                                 NumericVector& x_range, NumericVector& y_range,
                                 NumericVector& xlim, NumericVector& ylim,
                                 NumericVector& x, NumericVector& y, NumericVector& on,
                                 NumericVector& size, CharacterVector& glyph) {
  int n = x.size();
  int id_x = 0;
  int id_y = 0;

  double xmin = x_range[0];
  double xmax = x_range[1];
  double ymin = y_range[0];
  double ymax = y_range[1];

  NumericMatrix R(plot_height, plot_width);

  bool is_on = true;
  if(on.size() == 0) is_on = false;
  bool is_size = true;
  if(size.size() == 0) is_size = false;

  // remove unnecessary loop by logical check
  if(is_size) {

    for(int i=0; i < n; i++) {

      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {

        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));

        if(glyph[0] == "square") {

          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            for(int k = id_x - size[i]; k < id_x + size[i] + 1; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                if(is_on) {
                  if(on[i] != 0) {
                    R(j, k) = 1;
                  }
                } else {
                  R(j, k) = 1;
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
                    R(j, k) = 1;
                  }
                } else {
                  R(j, k) = 1;
                }
              }
            }
          }
        }
      }
    }

  } else {

    for(int i=0; i < n; i++) {

      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));

        if(is_on) {
          if(on[i] != 0) {
            R(id_y, id_x) = 1;
          }
        } else {
          R(id_y, id_x) = 1;
        }
      }
    }
  }

  return(R);
}


//[[Rcpp::export]]
NumericMatrix aggregation_meanCpp(int& plot_width, int& plot_height,
                                  NumericVector& x_range, NumericVector& y_range,
                                  NumericVector& xlim, NumericVector& ylim,
                                  NumericVector& x, NumericVector& y, NumericVector& on,
                                  NumericVector& size, CharacterVector& glyph) {
  int n = x.size();
  int id_x = 0;
  int id_y = 0;

  double xmin = x_range[0];
  double xmax = x_range[1];
  double ymin = y_range[0];
  double ymax = y_range[1];

  NumericMatrix R(plot_height, plot_width);
  bool is_size = true;
  if(size.size() == 0) is_size = false;

  // remove unnecessary loop by logical check
  if(is_size) {

    for(int i=0; i < n; i++) {

      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {

        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));

        if(glyph[0] == "square") {

          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            for(int k = id_x - size[i]; k < id_x + size[i] + 1; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                R(j, k) = on[i];
              }
            }
          }

        } else if(glyph[0] == "circle") {

          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            int m = abs(id_y - j);
            for(int k = id_x - size[i] + m; k < id_x + size[i] + 1 - m; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                R(j, k) = on[i];
              }
            }
          }
        }
      }
    }

  } else {

    for(int i=0; i < n; i++) {

      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));

        R(id_y, id_x) = on[i];
      }
    }
  }

  return(R);
}

//[[Rcpp::export]]
NumericMatrix aggregation_firstCpp(int& plot_width, int& plot_height,
                                   NumericVector& x_range, NumericVector& y_range,
                                   NumericVector& xlim, NumericVector& ylim,
                                   NumericVector& x, NumericVector& y, NumericVector& on,
                                   NumericVector& size, CharacterVector& glyph) {
  int n = x.size();
  int id_x = 0;
  int id_y = 0;

  double xmin = x_range[0];
  double xmax = x_range[1];
  double ymin = y_range[0];
  double ymax = y_range[1];

  NumericMatrix R(plot_height, plot_width);

  bool is_size = true;
  if(size.size() == 0) is_size = false;

  // remove unnecessary loop by logical check
  if(is_size) {

    for(int i= (n-1); i >= 0; i--) {

      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {

        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));

        if(glyph[0] == "square") {
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            for(int k = id_x - size[i]; k < id_x + size[i] + 1; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                R(j, k) = on[i];
              }
            }
          }
        } else if(glyph[0] == "circle") {
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            int m = abs(id_y - j);
            for(int k = id_x - size[i] + m; k < id_x + size[i] + 1 - m; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                R(j, k) = on[i];
              }
            }
          }
        }
      }
    }

  } else {

    for(int i= (n-1); i >= 0; i--) {

      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));
        R(id_y, id_x) = on[i];
      }
    }
  }

  return(R);
}


//[[Rcpp::export]]
NumericMatrix aggregation_lastCpp(int& plot_width, int& plot_height,
                                  NumericVector& x_range, NumericVector& y_range,
                                  NumericVector& xlim, NumericVector& ylim,
                                  NumericVector& x, NumericVector& y, NumericVector& on,
                                  NumericVector& size, CharacterVector& glyph) {
  int n = x.size();
  int id_x = 0;
  int id_y = 0;

  double xmin = x_range[0];
  double xmax = x_range[1];
  double ymin = y_range[0];
  double ymax = y_range[1];

  NumericMatrix R(plot_height, plot_width);

  bool is_size = true;
  if(size.size() == 0) is_size = false;

  // remove unnecessary loop by logical check
  if(is_size) {

    for(int i= 0; i < n; i++) {

      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {

        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));

        if(glyph[0] == "square") {
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            for(int k = id_x - size[i]; k < id_x + size[i] + 1; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                R(j, k) = on[i];
              }
            }
          }
        } else if(glyph[0] == "circle") {
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            int m = abs(id_y - j);
            for(int k = id_x - size[i] + m; k < id_x + size[i] + 1 - m; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                R(j, k) = on[i];
              }
            }
          }
        }
      }
    }

  } else {

    for(int i= 0; i < n; i++) {

      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));
        R(id_y, id_x) = on[i];
      }
    }
  }

  return(R);
}


//[[Rcpp::export]]
NumericMatrix aggregation_maxCpp(int& plot_width, int& plot_height,
                                 NumericVector& x_range, NumericVector& y_range,
                                 NumericVector& xlim, NumericVector& ylim,
                                 NumericVector& x, NumericVector& y, NumericVector& on,
                                 NumericVector& size, CharacterVector& glyph) {
  int n = x.size();
  int id_x = 0;
  int id_y = 0;

  double xmin = x_range[0];
  double xmax = x_range[1];
  double ymin = y_range[0];
  double ymax = y_range[1];

  NumericMatrix R(plot_height, plot_width);
  
  // make on is larger than zero;
  double min_on = min(on);
  on = on - min_on + 1e-8;
  
  bool is_size = true;
  if(size.size() == 0) is_size = false;

  // remove unnecessary loop by logical check
  if(is_size) {

    for(int i= 0; i < n; i++) {

      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {

        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));

        if(glyph[0] == "square") {
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            for(int k = id_x - size[i]; k < id_x + size[i] + 1; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                if(R(j, k) < on[i]) R(j, k) = on[i];
              }
            }
          }
        } else if(glyph[0] == "circle") {

          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            int m = abs(id_y - j);
            for(int k = id_x - size[i] + m; k < id_x + size[i] + 1 - m; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                if(R(j, k) < on[i]) R(j, k) = on[i];
              }
            }
          }
        }
      }
    }

  } else {

    for(int i= 0; i < n; i++) {

      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));
        if(R(id_y, id_x) < on[i]) R(id_y, id_x) = on[i];
      }
    }
  }

  return(R + min_on);
}

//[[Rcpp::export]]
NumericMatrix aggregation_minCpp(int& plot_width, int& plot_height,
                                 NumericVector& x_range, NumericVector& y_range,
                                 NumericVector& xlim, NumericVector& ylim,
                                 NumericVector& x, NumericVector& y, NumericVector& on,
                                 NumericVector& size, CharacterVector& glyph) {
  int n = x.size();
  int id_x = 0;
  int id_y = 0;

  double xmin = x_range[0];
  double xmax = x_range[1];
  double ymin = y_range[0];
  double ymax = y_range[1];

  NumericMatrix R(plot_height, plot_width);

  bool is_size = true;
  if(size.size() == 0) is_size = false;
  
  // make on is smaller than zero;
  double max_on = max(on);
  on = on - max_on - 1e-8;

  // remove unnecessary loop by logical check
  if(is_size) {

    for(int i= 0; i < n; i++) {

      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {

        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));

        if(glyph[0] == "square") {
          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            for(int k = id_x - size[i]; k < id_x + size[i] + 1; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                if(R(j, k) > on[i]) R(j, k) = on[i];
              }
            }
          }
        } else if(glyph[0] == "circle") {

          for(int j = id_y - size[i]; j < id_y + size[i] + 1; j++) {
            int m = abs(id_y - j);
            for(int k = id_x - size[i] + m; k < id_x + size[i] + 1 - m; k++) {
              if(k>=0 && j>=0 && k <= (plot_width-1) && j <= (plot_height-1)) {
                if(R(j, k) > on[i]) R(j, k) = on[i];
              }
            }
          }
        }
      }
    }

  } else {

    for(int i= 0; i < n; i++) {

      if(x[i] <= xlim[1] && x[i] >= xlim[0] && y[i] <= ylim[1] && y[i] >= ylim[0]) {
        id_x = floor((x[i] - xmin)/(xmax - xmin) * (plot_width - 1));
        id_y = floor((y[i] - ymin)/(ymax - ymin) * (plot_height - 1));
        if(R(id_y, id_x) > on[i]) R(id_y, id_x) = on[i];
      }
    }
  }

  return(R + max_on);
}