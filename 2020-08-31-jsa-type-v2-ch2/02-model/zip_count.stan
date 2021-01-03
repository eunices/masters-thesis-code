functions { 

	vector count_series_lp(

		// data for spp. 
		int[] y, 

		// data for publications as offset,
		int[] off,

		 // temporal correlation
		real phi,
		vector delta, real alpha, real beta, 

		// zero inflation
		real gamma, real eta
		
	) {   

		int Y;

		real value;
		real lp;
		vector[size(y)] loglik;

		vector[size(y)] lambda;
		vector[size(y)] omega;

		vector[size(y)] theta;

		Y = size(y);

		// temporal correlation
		lambda[1] = phi;
		for(i in 2:Y) {
			
			lambda[i] = exp(delta[1] + delta[2] * i) + // model
						alpha * y[i - 1] +             // short term lag
						beta * lambda[i - 1];          // long term trend
		
		}

		// zero inflation
		theta[1] = 0;
		for(i in 2:Y) {
			
			// dependent on previous time step
			// two state markov process
			if(y[i-1] == 0) { 
				theta[i] = gamma; 
			} else {
				theta[i] = eta;
			}
			
		}

		// log prob
		for(i in 1:Y) {
			if(y[i] == 0) {

				loglik[i] = 
					log_sum_exp(
						bernoulli_lpmf( 1 | theta[i] ),
						
						bernoulli_lpmf( 0 | theta[i] ) + 
						poisson_lpmf( y[i] | (off[i] + 1) * lambda[i] )
					);
				

			} else {

				loglik[i] = 
					 bernoulli_lpmf( 0 | theta[i] ) +
					 poisson_lpmf( y[i] | (off[i] + 1) * lambda[i] );
				
			}

			// lp += value;
			

		}

		return loglik;
	}

}


data {

	int<lower=0> N; // years
	int<lower=0> P; // groups
	
	// index where value is != 0 of each group
	int<lower=0> str[P];  
	// length of each group
	int<lower=0> end[P];

	// N species data [group x years]
	int<lower=0> counts[P, N]; 
	// N publications data [group x years]
	int<lower=0> off[P, N];
}


parameters {

	vector[2] delta[P];
	vector[2] mu;
	corr_matrix[2] Omega;
	vector<lower=0>[2] tau;
	real<lower=0,upper=1> alpha[P];
	real<lower=0,upper=1> beta_unc[P];
	real<lower=0,upper=1> gamma[P];
	real<lower=0,upper=1> eta[P];

	real<lower=0> phi[P];    // param at t = 1
	real mu_phi;             // param at t = 1
	real<lower=0> sigma_phi; // param at t = 1

}


transformed parameters {

	real<lower=0,upper=1> beta[P];
	cov_matrix[2] Sigma;
	matrix[P, N] log_lik;

	for(p in 1:P) {
		beta[p] = (1 - alpha[p]) * beta_unc[p];
	}

	Sigma = quad_form_diag(Omega, tau);

	// Update log prob
	for(p in 1:P) {

		log_lik[p] =  to_row_vector(
			append_row(

				// pad log_lik_row with 0
				rep_vector(0, (N - size(counts[p][str[p]:end[p]]))),
				count_series_lp(
				
				counts[p][str[p]:end[p]],
				off[p][str[p]:end[p]], 

				phi[p],
				delta[p], 
				alpha[p], 
				beta[p], 
				
				gamma[p], 
				eta[p]

				)
			)
		);

	}

}


model {

	// ACP
	for(p in 1:P) {
		alpha[p] ~ beta(1, 3);
		beta_unc[p] ~ beta(1, 3);
	}

	// ACP t=1
	mu_phi ~ normal(0, 1);
	sigma_phi ~ cauchy(0, 1);
	phi ~ lognormal(mu_phi, sigma_phi);

	// ACP t>1
	mu[1] ~ normal(0, 5);
	mu[2] ~ normal(0, 1); 

	tau ~ cauchy(0, 1);
	Omega ~ lkj_corr(2);

	for(p in 1:P) {
		delta[p] ~ multi_normal(mu, Sigma);
	}

	// Update log prob
	for(p in 1:P) {
		target += sum(count_series_lp(
			
			counts[p][str[p]:end[p]],
			off[p][str[p]:end[p]], 

			phi[p],
			delta[p], 
			alpha[p], 
			beta[p], 
			
			gamma[p], 
			eta[p]

		));
	}

}

generated quantities {

}
