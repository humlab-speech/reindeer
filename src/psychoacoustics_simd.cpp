// SIMD-optimized psychoacoustic functions using RcppXsimd
// Provides high-performance vectorized implementations of st() and erb()

#include <Rcpp.h>
#include <xsimd/xsimd.hpp>

using namespace Rcpp;
namespace xs = xsimd;

//' SIMD-optimized semitone conversion
//'
//' Convert frequencies (Hz) to semitones using vectorized operations.
//' Uses SIMD instructions for log, division, and multiplication.
//'
//' @param x NumericVector of frequencies in Hz
//' @param ref Reference frequency (default 16.35160 Hz = C0)
//' @return NumericVector of semitone values
//' @keywords internal
// [[Rcpp::export]]
NumericVector st_simd(NumericVector x, double ref = 16.35160) {
  const int n = x.size();
  NumericVector out(n);

  // Constants for the calculation: 12 * log(x/ref) / log(2)
  const double log2_val = std::log(2.0);
  const double coef = 12.0 / log2_val;

  // Get raw pointers
  const double* x_ptr = &x[0];
  double* out_ptr = &out[0];

  // Define SIMD batch type (uses optimal width for current architecture)
  // ARM NEON: 128-bit / 64-bit double = 2 elements
  // AVX: 256-bit / 64-bit double = 4 elements
  // AVX-512: 512-bit / 64-bit double = 8 elements
  #if defined(__ARM_NEON) || defined(__aarch64__)
    using batch_type = xs::batch<double, 2>;
  #elif defined(__AVX512F__)
    using batch_type = xs::batch<double, 8>;
  #elif defined(__AVX__)
    using batch_type = xs::batch<double, 4>;
  #elif defined(__SSE2__)
    using batch_type = xs::batch<double, 2>;
  #else
    using batch_type = xs::batch<double, 2>;  // Fallback
  #endif
  constexpr std::size_t simd_size = batch_type::size;

  // Process SIMD-width chunks
  const std::size_t vec_size = n - (n % simd_size);

  for (std::size_t i = 0; i < vec_size; i += simd_size) {
    // Load SIMD batch
    batch_type batch_x;
    batch_x.load_unaligned(&x_ptr[i]);

    // Compute: x / ref
    auto batch_div = batch_x / ref;

    // Compute: log(x / ref)
    auto batch_log = xs::log(batch_div);

    // Compute: 12 * log(x / ref) / log(2)
    auto batch_result = batch_log * coef;

    // Store result
    batch_result.store_unaligned(&out_ptr[i]);
  }

  // Process remaining elements (scalar fallback)
  for (std::size_t i = vec_size; i < static_cast<std::size_t>(n); ++i) {
    out_ptr[i] = coef * std::log(x_ptr[i] / ref);
  }

  return out;
}

//' SIMD-optimized ERB scale conversion
//'
//' Convert frequencies (Hz) to Equivalent Rectangular Bandwidth (ERB) scale
//' using vectorized operations. Formula from Moore (1982).
//'
//' @param f NumericVector of frequencies in Hz
//' @return NumericVector of ERB values
//' @keywords internal
// [[Rcpp::export]]
NumericVector erb_simd(NumericVector f) {
  const int n = f.size();
  NumericVector out(n);

  // Constants for the calculation: 11.17 * log((f + 0.312) / (f + 14.675)) + 43
  const double c1 = 0.312;
  const double c2 = 14.675;
  const double coef = 11.17;
  const double offset = 43.0;

  // Get raw pointers
  const double* f_ptr = &f[0];
  double* out_ptr = &out[0];

  // Define SIMD batch type
  #if defined(__ARM_NEON) || defined(__aarch64__)
    using batch_type = xs::batch<double, 2>;
  #elif defined(__AVX512F__)
    using batch_type = xs::batch<double, 8>;
  #elif defined(__AVX__)
    using batch_type = xs::batch<double, 4>;
  #elif defined(__SSE2__)
    using batch_type = xs::batch<double, 2>;
  #else
    using batch_type = xs::batch<double, 2>;  // Fallback
  #endif
  constexpr std::size_t simd_size = batch_type::size;

  // Process SIMD-width chunks
  const std::size_t vec_size = n - (n % simd_size);

  for (std::size_t i = 0; i < vec_size; i += simd_size) {
    // Load SIMD batch
    batch_type batch_f;
    batch_f.load_unaligned(&f_ptr[i]);

    // Compute numerator: f + 0.312
    auto batch_num = batch_f + c1;

    // Compute denominator: f + 14.675
    auto batch_denom = batch_f + c2;

    // Compute: (f + 0.312) / (f + 14.675)
    auto batch_div = batch_num / batch_denom;

    // Compute: log((f + 0.312) / (f + 14.675))
    auto batch_log = xs::log(batch_div);

    // Compute: 11.17 * log(...) + 43
    auto batch_result = (batch_log * coef) + offset;

    // Store result
    batch_result.store_unaligned(&out_ptr[i]);
  }

  // Process remaining elements (scalar fallback)
  for (std::size_t i = vec_size; i < static_cast<std::size_t>(n); ++i) {
    double num = f_ptr[i] + c1;
    double denom = f_ptr[i] + c2;
    out_ptr[i] = coef * std::log(num / denom) + offset;
  }

  return out;
}

//' Check if SIMD is available and get architecture info
//'
//' Returns information about the SIMD capabilities available at runtime.
//' Useful for debugging and verification.
//'
//' @return List with SIMD architecture information
//' @export
// [[Rcpp::export]]
List simd_info() {
  List info;

  #if defined(__AVX512F__)
    info["arch"] = "AVX-512";
    info["width"] = 512;
  #elif defined(__AVX2__)
    info["arch"] = "AVX2";
    info["width"] = 256;
  #elif defined(__AVX__)
    info["arch"] = "AVX";
    info["width"] = 256;
  #elif defined(__SSE4_2__)
    info["arch"] = "SSE4.2";
    info["width"] = 128;
  #elif defined(__SSE4_1__)
    info["arch"] = "SSE4.1";
    info["width"] = 128;
  #elif defined(__ARM_NEON)
    info["arch"] = "ARM NEON";
    info["width"] = 128;
  #else
    info["arch"] = "scalar (no SIMD)";
    info["width"] = 64;
  #endif

  #if defined(__ARM_NEON) || defined(__aarch64__)
    using batch_type = xs::batch<double, 2>;
  #elif defined(__AVX512F__)
    using batch_type = xs::batch<double, 8>;
  #elif defined(__AVX__)
    using batch_type = xs::batch<double, 4>;
  #elif defined(__SSE2__)
    using batch_type = xs::batch<double, 2>;
  #else
    using batch_type = xs::batch<double, 2>;  // Fallback
  #endif
  info["batch_size"] = (int)batch_type::size;
  info["simd_enabled"] = true;

  return info;
}
