import pyhydrophone as pyhy
import soundfile as sf
import pathlib
import pandas as pd
import numpy as np
import scipy.signal as sig
import matplotlib.pyplot as plt
import maad
import json
import seaborn as sns


LOW_FREQ = [0, 1000]
MID_FREQ = [1000, 5000]
HIGH_FREQ = [5000, 10000]

NFFT = 4096


def compute_psd(signal_recorded, fs_recorded):
    window = sig.get_window('hann', NFFT)

    # Compute the psd
    freq_, psd_ = sig.welch(signal_recorded, fs=fs_recorded, window=window, nfft=NFFT,
                            scaling='density', noverlap=int(NFFT/2))
    return freq_, psd_


def compute_frequencies(freq, psd):
    low = 10 * np.log10(psd[(freq >= LOW_FREQ[0]) & (freq <= LOW_FREQ[1])].mean())
    mid = 10 * np.log10(psd[(freq >= MID_FREQ[0]) & (freq <= MID_FREQ[1])].mean())
    high = 10 * np.log10(psd[(freq >= HIGH_FREQ[0]) & (freq <= HIGH_FREQ[1])].mean())

    return low, mid, high


def compute_spl(signal_recorded):
    # Compute the psd
    spl_db = 10 * np.log10((signal_recorded ** 2).mean())

    return spl_db


def compute_indices(signal_recorded, fs_recorded):
    sxx, _, f, _ = maad.sound.spectrogram(signal_recorded, fs_recorded, window='hann',
                                          nperseg=NFFT, noverlap=int(NFFT/2), mode='psd')
    _, _ , aci_value = maad.features.acoustic_complexity_index(sxx)
    adi = maad.features.acoustic_diversity_index(sxx, f)
    aei = maad.features.acoustic_eveness_index(sxx, f)
    return aci_value, adi, aei

output_folder = pathlib.Path("../figures")
results_path = output_folder.joinpath('results_spl.csv')
results_original_path = output_folder.joinpath('results_spl_original_files.csv')

all_treatment_names = ['reef', 'off_reef', 'boat']
columns = ['batch', 'treatment', 'spl', 'low_freq', 'mid_freq', 'high_freq', 'aci', 'adi', 'aei']

if not results_path.exists():
    recorded_path = input('Path to 1h recorded files:')
    original_path = input('Path to the files used for the experiment:')
    recorded_levels_folder = pathlib.Path(recorded_path)
    original_levels_folder = pathlib.Path(original_path)
    metadata_path = original_levels_folder.joinpath('metadata.json')

    f = open(metadata_path)
    metadata = json.load(f)
    f.close()

    p_ref = 1.0
    hy_sens = -158
    Vpp = 2.0

    aquarium_hydrophone = pyhy.icListen(name='aquarium', model='scientific', serial_number=1,
                                        sensitivity=hy_sens, preamp_gain=0, Vpp=Vpp)

    results_df = pd.DataFrame(columns=columns)
    results_original = pd.DataFrame(columns=columns)

    cups = np.arange(5)

    for batch_folder in recorded_levels_folder.glob('*'):
        batch = batch_folder.name

        for file_path in batch_folder.glob('*.wav'):
            treatment_name = file_path.name.split('.wav')[0]

            # Convert to upa
            gain_upa_db_aq = aquarium_hydrophone.end_to_end_calibration()

            s, fs = sf.read(file_path, always_2d=True)
            s = s[:, 0]

            ma = 10 ** (gain_upa_db_aq / 20.0)
            s_upa = s * ma

            # Read the recorded files
            f, psd = compute_psd(s_upa, fs)

            spl = compute_spl(s_upa)

            low_freq_spl, mid_freq_spl, high_freq_spl = compute_frequencies(f, psd)

            aci, adi, aei = compute_indices(s_upa, fs)

            i = len(results_df)
            results_df.loc[i, columns] = [batch, treatment_name, spl, low_freq_spl,
                                          mid_freq_spl, high_freq_spl, aci, adi, aei]
            results_df.loc[i, f] = 10 * np.log10(psd)

        batch_folder_original = original_levels_folder.joinpath(batch)
        for file_path in batch_folder_original.glob('*.wav'):
            wav_file_name = file_path.name.split('.wav')[0]
            if 'REEF_AND_BOAT' not in wav_file_name:
                for t in all_treatment_names:
                    if t in wav_file_name.lower():
                        treatment_name_original = t
                params = metadata[file_path.name]
                hydrophone_class = getattr(pyhy, params['hydrophone']['name'])
                hydrophone = hydrophone_class(**params['hydrophone'])
                amplif = params['amplification']

                # Convert to upa
                gain_upa_db = hydrophone.end_to_end_calibration()
                print(params['hydrophone'], gain_upa_db)

                s, fs = sf.read(file_path, always_2d=True)
                s = s[:, 0]

                ma = 10 ** (gain_upa_db / 20.0)
                s_upa = s * ma

                # Read the recorded files
                f, psd = compute_psd(s_upa, fs)

                spl = compute_spl(s_upa)

                low_freq_spl, mid_freq_spl, high_freq_spl = compute_frequencies(f, psd)

                aci, adi, aei = compute_indices(s_upa, fs)

                i = len(results_original)
                results_original.loc[i, columns] = [batch, treatment_name_original, spl, low_freq_spl,
                                                    mid_freq_spl, high_freq_spl, aci, adi, aei]
                results_original.loc[i, f] = 10 * np.log10(psd)

    results_df.to_csv(results_path, index=False)
    results_original.to_csv(results_original_path, index=False)

else:
    results_df = pd.read_csv(results_path, index_col=False)
    results_original = pd.read_csv(results_original_path, index_col=False)


results_df['origin'] = 'experiment'
results_original['origin'] = 'field'
total_results = pd.concat([results_df, results_original])
freq_cols = []
freq_vals = []
max_freq = 10000
for col in total_results.columns:
    if col not in (columns + ['origin']):
        freq_cols.append(col)
        freq_vals.append(float(col))

n_batches = len(results_df.batch.unique())

# Select and plot the psd
total_results = total_results.replace(['no_sound', 'off_reef', 'reef', 'boat', 'reef_and_boat'],
                                      ['no sound', 'off reef', 'reef', 'vessel', 'reef and vessel'])
selected_freqs = list(np.array(freq_cols)[np.array(freq_vals) < max_freq])
psd_results = total_results[selected_freqs + ['batch', 'origin', 'treatment']]
psd_results = psd_results.melt(id_vars=['origin', 'batch', 'treatment'], value_vars=selected_freqs,
                               var_name='freq', value_name='spl')
psd_results['freq'] = psd_results['freq'].astype(float)

plt.rcParams.update({'font.size': 22,
                     'lines.linewidth': 4})
g = sns.FacetGrid(psd_results, col='origin', row='batch', hue='treatment', sharex=True,
                  sharey=True, xlim=(20, max_freq), height=4, aspect=1.5, palette='colorblind')
g.map(sns.lineplot, 'freq', 'spl')
g.set(xscale='symlog')
g.fig.supylabel(r'PSD [dB re $1 \mu Pa^2Hz^{-1}$]')
g.fig.supxlabel('Frequency [Hz]')
g.set_axis_labels('', '')
g.set_titles('{col_name} | {row_name}')
g.add_legend(title='Treatment', label_order=['reef', 'reef and vessel', 'vessel', 'off reef', 'no sound'])
plt.savefig(output_folder.joinpath('figure_summary_psd.png'))
plt.show()

# Plot the metrics
acu_columns = list(set(columns) - set(['batch', 'origin', 'treatment']))
metrics_results = total_results.melt(id_vars=['origin', 'treatment', 'batch'], value_vars=acu_columns,
                                     var_name='feature', value_name='value')

# Get ADI out
# metrics_results = metrics_results.loc[metrics_results.feature != 'adi']
metrics_results["feature"] = metrics_results["feature"].map({'aci': "ACI", 'aei': "AEI", 'spl': 'SPL', 'adi': 'ADI',
                                                             'low_freq': 'low frequency', 'mid_freq': 'mid frequency',
                                                             'high_freq': 'high frequency'})
metrics_results["treatment"] = metrics_results["treatment"].map({'no sound': 'NS', 'vessel': 'V', 'off reef': 'OFF', 'reef': 'R',
                                                                 'reef and vessel': 'R+V'})

plt.rcParams.update({'font.size': 24})
fig, ax = plt.subplots(2, 4, figsize=(24, 12), sharex=True)
sns.boxplot(metrics_results.loc[metrics_results['feature'] == 'low frequency'], x='treatment', y='value', ax=ax[1][0],
            hue='origin', order=['R', 'R+V', 'V', 'OFF', 'NS'], palette='colorblind', legend=False)
ax[1][0].set_ylim([20, 100])
ax[1][0].set_title('low frequency PSD')
ax[1][0].set_ylabel('Average PSD [dB re $1 \mu Pa^2Hz^{-1}$]')
sns.boxplot(metrics_results.loc[metrics_results['feature'] == 'mid frequency'], x='treatment', y='value', ax=ax[1][1],
            hue='origin', order=['R', 'R+V', 'V', 'OFF', 'NS'], palette='colorblind', legend=False)
ax[1][1].set_ylim([20, 100])
ax[1][1].set_title('mid frequency PSD')
ax[1][1].set_ylabel('Average PSD [dB re $1 \mu Pa^2Hz^{-1}$]')
sns.boxplot(metrics_results.loc[metrics_results['feature'] == 'high frequency'], x='treatment', y='value', ax=ax[1][2],
            hue='origin', order=['R', 'R+V', 'V', 'OFF', 'NS'], palette='colorblind', legend=False)
ax[1][2].set_ylim([20, 100])
ax[1][2].set_title('high frequency PSD')
ax[1][2].set_ylabel('Average PSD [dB re $1 \mu Pa^2Hz^{-1}$]')
sns.boxplot(metrics_results.loc[metrics_results['feature'] == 'SPL'], x='treatment', y='value', ax=ax[1][3],
            hue='origin', order=['R', 'R+V', 'V', 'OFF', 'NS'], palette='colorblind', legend=False)
ax[1][3].set_ylim([60, 130])
ax[1][3].set_title('Broadband Sound Level')
ax[1][3].set_ylabel('$SL_{rms}$ [dB re $1 \mu Pa^2$]')
sns.boxplot(metrics_results.loc[metrics_results['feature'] == 'ACI'], x='treatment', y='value', ax=ax[0][0],
            hue='origin', order=['R', 'R+V', 'V', 'OFF', 'NS'], palette='colorblind', legend=False)
ax[0][0].set_ylabel('Index value [unitless]')
ax[0][0].set_title('ACI')
sns.boxplot(metrics_results.loc[metrics_results['feature'] == 'ADI'], x='treatment', y='value', ax=ax[0][1],
            hue='origin', order=['R', 'R+V', 'V', 'OFF', 'NS'], palette='colorblind', legend=False)
ax[0][1].set_ylabel('Index value [unitless]')
ax[0][1].set_title('ADI')
g = sns.boxplot(metrics_results.loc[metrics_results['feature'] == 'AEI'], x='treatment', y='value', ax=ax[0][2],
            hue='origin', order=['R', 'R+V', 'V', 'OFF', 'NS'], palette='colorblind', legend=True)
ax[0][2].set_title('AEI')
ax[0][2].set_ylabel('Index value [unitless]')

lines, handles = ax[0][2].get_legend_handles_labels()
g.legend().set_visible(False)
ax[0][2].set_ylabel('Index value [unitless]')
ax[0][3].axis('off')
ax[0][3].legend(lines, handles)
plt.tight_layout()
plt.show()

# g = sns.catplot(
#     data=metrics_results, x='treatment', y='value', col='feature', hue='origin',
#     kind='box', col_wrap=3, sharex=True, sharey=False, height=5, aspect=1.2,
#     col_order=['low frequency', 'mid frequency', 'high frequency', 'SPL', 'ACI', 'AEI'],
#     order=['no sound', 'off reef', 'reef', 'boat', 'reef and_boat'], palette='colorblind'
# )
# g.set_axis_labels('', 'Value')
# g.set_xticklabels(['NS', 'OFF', 'R', 'B', 'R+B'])
# g.set_titles('{col_name}')
# plt.savefig(output_folder.joinpath('figure_summary_metrics.png'))
# plt.show()
