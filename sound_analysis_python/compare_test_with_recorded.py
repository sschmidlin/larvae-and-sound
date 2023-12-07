import pyhydrophone as pyhy
import json
import soundfile as sf
import pathlib
import pypam
import matplotlib.pyplot as plt
import numpy as np
import scipy.signal as sig


def compute_psd(sound_file, hy):
    signal_recorded, fs_recorded = sf.read(sound_file, always_2d=True, frames=48000*60)
    ma = 10 ** (hy.end_to_end_calibration() / 20.0)
    s = signal_recorded[:, 0] / ma
    nfft = 4096
    window = sig.get_window('hann', nfft)

    # Compute the psd
    freq_, psd_ = sig.welch(s, fs=fs_recorded, window=window, nfft=nfft,
                            scaling='density', noverlap=0.5)

    psd_db = 10 * np.log10(psd_)

    return freq_, psd_db


original_path = input('original files folder (one batch):')
recorded_path = input('recorded files (one batch)')
test_levels_folder = pathlib.Path(original_path)
recorded_levels_folder = pathlib.Path(recorded_path)
metadata_path = test_levels_folder.joinpath('metadata.json')

f = open(metadata_path)
metadata = json.load(f)
f.close()

p_ref = 1.0
hy_sens = -158
Vpp = 2.0

aquarium_hydrophone = pyhy.icListen(name='aquarium', model='scientific', serial_number=1,
                                    sensitivity=hy_sens, preamp_gain=0, Vpp=Vpp)

spectrums = {}

for file_name, params in metadata.items():
    file_path = metadata_path.parent.joinpath(file_name)
    treatment_name = file_name.split('.wav')[0]

    files_to_compare = []
    for recorded_file in recorded_levels_folder.glob('*.wav'):
        if treatment_name in recorded_file.name:
            files_to_compare.append(recorded_file)
    hydrophone_class = getattr(pyhy, params['hydrophone']['name'])
    hydrophone = hydrophone_class(**params['hydrophone'])

    # Convert to upa
    freq, psd = compute_psd(file_path, hy=hydrophone)

    # Calibrate the psd
    amplif = params['amplification']
    spectrums[treatment_name] = psd - amplif
    if len(files_to_compare) > 0:

        fig, ax = plt.subplots()
        ax.plot(freq, psd - amplif, label=treatment_name)
        for recorded_file in files_to_compare:
            # Read the recorded files
            freq_recorded, psd_recorded = compute_psd(recorded_file, hy=aquarium_hydrophone)

            # Calibrate the psd
            ax.plot(freq_recorded, psd_recorded, label=recorded_file.name)

        fan_freq, fan_psd = compute_psd(recorded_levels_folder.joinpath('no_sound_treatment.wav'),
                                        hy=aquarium_hydrophone)
        ax.plot(fan_freq, fan_psd, label='no sound treatment with others on')

        fan_freq, fan_psd = compute_psd(recorded_levels_folder.joinpath('room_noise_silent.wav'),
                                        hy=aquarium_hydrophone)
        ax.plot(fan_freq, fan_psd, label='room noise')

        plt.xlim([0, 10000])
        plt.xlabel('Frequency [Hz]')
        plt.ylabel('SPL')
        plt.legend()
        plt.show()


fig, ax = plt.subplots()
for treatment_name, psd in spectrums.items():
    ax.plot(freq, psd, label=treatment_name)
plt.xlim([0, 10000])
plt.xlabel('Frequency [Hz]')
plt.ylabel('SPL')
plt.legend()
plt.show()

