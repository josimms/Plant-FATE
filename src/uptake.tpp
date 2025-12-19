#ifndef PLANT_UPTAKE_TPP
#define PLANT_UPTAKE_TPP

namespace plant{

template<class _Climate>
void Uptake::nitrogen_plant(_Climate C, PlantArchitecture& G, PlantTraits& T) {
  double N = C.clim_acclim.nitrogen;
  double root_length = G.root_length;
  double root_no = G.root_no;
  
  double age_effect = uptake_age(G, T);
  
  double uptake_roots_term = (1.0 - mycorrhized) * uptake_roots(N, G, T);
  double uptake_mycorrhiza_term = mycorrhized * nitrogen_gate(G, T) * investment_from_myco * uptake_myco(N, G);
  
  G.nitrogen_tree = (uptake_roots_term + uptake_mycorrhiza_term) / 1000.0;
  G.potential_nitrogen_leaf = T.k_10 * G.nitrogen_tree / (G.crown_area * G.lai);
}

} // namespace plant

#endif // PLANT_UPTAKE_TPP