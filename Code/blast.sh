#!/bin/bash
#SBATCH --job-name=blastASV		# Job name (testBowtie2)
#SBATCH --partition=batch		# Partition name (batch, highmem_p, or gpu_p)
#SBATCH --ntasks=1			# Run job in single task, by default using 1 CPU core on a single node
#SBATCH --cpus-per-task=8	 	# CPU core count per task, by default 1 CPU core per task
#SBATCH --mem=80G			# Memory per node (4GB); by default using M as unit
#SBATCH --time=5:00:00              	# Time limit hrs:min:sec or days-hours:minutes:seconds
#SBATCH --output=%x_%j.out		# Standard output log, e.g., testBowtie2_12345.out
#SBATCH --error=%x_%j.err		# Standard error log, e.g., testBowtie2_12345.err
#SBATCH --mail-user=pedro.rodrigues@uga.edu    # Where to send mail
#SBATCH --mail-type=END,FAIL          	# Mail events (BEGIN, END, FAIL, ALL)


module load BLAST+/2.14.1-gompi-2023a
module load ncbiblastdb/20241102
mkdir blast_results
blastn -query MG_asv_04_2025.fasta -db nt -evalue 1e-30 -num_threads 8 -max_target_seqs 1 -outfmt '6 qseqid sseqid stitle evalue bitscore pident qcovs qcovhsp' -out blast_results/28APR2025_nt_blast_MG.tsv
