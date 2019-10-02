create table snp (
  snp_id UUID NOT NULL PRIMARY KEY,
	rsid VARCHAR(50) NOT NULL,
	chr	SMALLINT NOT NULL,
	pos INTEGER NOT NULL,
  ref CHAR(1),
  alt CHAR(1),
  typed BOOLEAN,
	info REAL,
	public_maf REAL,
	dbmt_maf REAL
);

CREATE INDEX idx_chr ON snp(chr);

create table outcome (
  outcome_id UUID NOT NULL PRIMARY KEY,
  genome VARCHAR(2) NOT NULL,
  outcome	VARCHAR(10) NOT NULL,
  censor_time VARCHAR(10) NOT NULL,
  disease_grp VARCHAR(15) NOT NULL,
  pt_subset VARCHAR(50),
  ethnicity VARCHAR(5) NOT NULL,
  n_c1 SMALLINT,
  n_c2 SMALLINT,
  nevent_c1 SMALLINT,
  nevent_c2 SMALLINT
);

 
create table results (
	PRIMARY KEY (snp_id, outcome_id),
	snp_id UUID REFERENCES snp (snp_id),
	outcome_id UUID REFERENCES outcome (outcome_id),
	alt_metal CHAR(1),
  coef_c1 REAL,
  coef_c2 REAL,
  coef_m REAL,
  se_coef_c1 REAL,
  se_coef_c2 REAL,
  se_coef_m REAL,
  pvalue_c1_nlog10 REAL,
  pvalue_c2_nlog10 REAL,
  pvalue_m_nlog10 REAL,
  samp_freq_alt_c1 REAL,
  samp_freq_alt_c2 REAL,
  hetisq REAL,
  hetpval REAL);
