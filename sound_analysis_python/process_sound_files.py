import pyhydrophone as pyhy
import json
import soundfile as sf
import pathlib
import pypam
import matplotlib.pyplot as plt
import scipy.signal as sig

path_str = input('metadata (json) file, where all the subfiles to process are also stored in separate folders')
metadata_path = pathlib.Path(path_str)
f = open(metadata_path)
metadata = json.load(f)
f.close()

p_ref = 1.0
desired_fs = 48000

band = [20, 12000]

fig, ax = plt.subplots()
for treatment_folder, params in metadata.items():
    folder_path = metadata_path.parent.joinpath(treatment_folder)
    hydrophone_class = getattr(pyhy, params['hydrophone']['name'])
    hydrophone = hydrophone_class(**params['hydrophone'])

    for file_path in folder_path.glob('*.wav'):

        processed_file = file_path.parent.joinpath(file_path.name.replace('.wav', '_processed.wav'))
        if '_processed' not in file_path.name:
            # Convert to upa
            signal_wav, fs = sf.read(file_path)
            signal = pypam.signal.Signal(signal=signal_wav, fs=fs, channel=0)

            # Downsample and filter
            signal.remove_dc()
            # signal.set_band([20, 12000], downsample=False)
            if fs == 96000:
                print(file_path.name, 'This file is too high')
                signal.signal = sig.resample_poly(signal.signal, up=1, down=2)
                signal.fs = desired_fs
                sosfilt = sig.butter(N=4, btype='bandpass', Wn=band, analog=False, output='sos', fs=desired_fs)
            elif fs == 24000:
                print(file_path.name, 'This file is too low')
                signal.signal = sig.resample(signal.signal, num=len(signal.signal) * 2)
                signal.fs = desired_fs
                sosfilt = sig.butter(N=4, btype='highpass', Wn=band[0], analog=False, output='sos', fs=desired_fs)

            elif fs == 48000:
                sosfilt = sig.butter(N=4, btype='bandpass', Wn=band, analog=False, output='sos', fs=desired_fs)
                print(file_path.name, 'This file is good!')
            else:
                print(file_path.name, 'This file is weird', fs)

            # filter the signal
            signal.signal = sig.sosfilt(sosfilt, signal.signal)

            # Save the downsampled and filtered file
            sf.write(processed_file, data=signal.signal, samplerate=desired_fs)

        # Compute the psd
        freq, psd, _ = signal.spectrum(scaling='density', nfft=fs, db=True, overlap=0.5)

        # Calibrate the psd
        gain_upa_db = hydrophone.end_to_end_calibration()
        ax.plot(freq, psd + gain_upa_db, label=file_path.name)

plt.xlabel('Frequency [Hz]')
plt.ylabel('SPL')
plt.legend()
plt.show()
