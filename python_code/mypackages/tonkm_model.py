# Author: Thomas Sonderegger (sonderegger@ifu.baug.ethz.ch)*
# 
# Calculation of corrected ton-kilometers per Functional Unit (FU)
# 
# This code calculates the corrected ton-kilometers according to the TONKM model (whereby the distance calculation and correction (per functional unit (FU)) as well as the calculation of effective weights including transmission from machine to tractor have already be done in the Excel template). Results are per FU so they can directly be used with the inventory results.

import os
import pandas as pd
import numpy as np

def run_tonkm_model(df):

    # Clear from empty lines:
    df2 = df[df['name'] != 0]
    data = df2[pd.notnull(df2['name'])].copy()

    # Calculate pressure-corrected ton-kilometers according to the Excel-Model for each machine (tractor front, tractor rear, and machine) and three soil layers for one ha. First read in the data columns needed as arrays:
    distance = np.array(data['distance'])
    w_front_full = np.array(data['w_front_full'])
    w_front_empty = np.array(data['w_front_empty'])
    w_rear_full = np.array(data['w_rear_full'])
    w_rear_empty = np.array(data['w_rear_empty'])
    w_machine_full = np.array(data['w_machine_full'])
    w_machine_empty = np.array(data['w_machine_empty'])
    axles = np.array(data['axles'])
    axles_test = axles.copy()
    axles[axles == 0] = 1  # to avoid division by 0
    p_front = np.array(data['pressure_machine1_front'])
    p_front[p_front == 0] = 1  # to allow for np.log10(p_front)
    p_rear = np.array(data['pressure_machine1_rear'])
    p_rear[p_rear == 0] = 1  # to allow for np.log10(p_rear)
    p_machine = np.array(data['pressure_machine2'])
    p_machine[p_machine == 0] = 1  # to allow for np.log10(p_trailer)

    # Calculate corrections for three soil layers:
    """ WITHOUT furrow wheel """

    # WEIGHT CORRECTION
    # corrected weight top-soil
    cwt_front = (w_front_full + w_front_empty)/ 2 / 1000
    cwt_rear = (w_rear_full + w_rear_empty) / 2 / 1000
    cwt_machine = (w_machine_full + w_machine_empty) / 2 / 1000

    # corrected weight mid-soil
    cwm_front_full = (w_front_full - 4000) / 1000
    cwm_front_empty = (w_front_empty - 4000) / 1000
    cwm_rear_full = (w_rear_full - 4000) / 1000
    cwm_rear_empty = (w_rear_empty - 4000) / 1000
    cwm_machine_full = (w_machine_full / axles - 4000) / 1000 * axles
    cwm_machine_empty = (w_machine_empty / axles - 4000) / 1000 * axles
    # set negative values to 0
    cwm_front_full[cwm_front_full < 0] = 0
    cwm_front_empty[cwm_front_empty < 0] = 0
    cwm_rear_full[cwm_rear_full < 0] = 0
    cwm_rear_empty[cwm_rear_empty < 0] = 0
    cwm_machine_full[cwm_machine_full < 0] = 0
    cwm_machine_empty[cwm_machine_empty < 0] = 0

    # corrected weight bottom-soil
    cwb_front_full = (w_front_full - 6000) / 1000
    cwb_front_empty = (w_front_empty - 6000) / 1000
    cwb_rear_full = (w_rear_full - 6000) / 1000
    cwb_rear_empty = (w_rear_empty - 6000) / 1000
    cwb_machine_full = (w_machine_full / axles - 6000) / 1000 * axles
    cwb_machine_empty = (w_machine_empty / axles - 6000) / 1000 * axles
    # set negative values to 0
    cwb_front_full[cwb_front_full < 0] = 0
    cwb_front_empty[cwb_front_empty < 0] = 0
    cwb_rear_full[cwb_rear_full < 0] = 0
    cwb_rear_empty[cwb_rear_empty < 0] = 0
    cwb_machine_full[cwb_machine_full < 0] = 0
    cwb_machine_empty[cwb_machine_empty < 0] = 0

    # TKM
    # tkm top-soil
    tkmt_front = distance * cwt_front
    tkmt_rear = distance * cwt_rear
    tkmt_machine = distance * cwt_machine
    # set mounted machines to 0
    tkmt_machine[axles_test == 0] = 0

    # tkm mid-soil
    tkmm_front = distance * (0.5 * cwm_front_full + 0.5 * cwm_front_empty)
    tkmm_rear = distance * (0.5 * cwm_rear_full + 0.5 * cwm_rear_empty)
    tkmm_machine = distance * (0.5 * cwm_machine_full + 0.5 * cwm_machine_empty)
    # set mounted machines to 0
    tkmm_machine[axles_test == 0] = 0

    # tkm bottom-soil
    tkmb_front = distance * (0.5 * cwb_front_full + 0.5 * cwb_front_empty)
    tkmb_rear = distance * (0.5 * cwb_rear_full + 0.5 * cwb_rear_empty)
    tkmb_machine = distance * (0.5 * cwb_machine_full + 0.5 * cwb_machine_empty)
    # set mounted machines to 0
    tkmb_machine[axles_test == 0] = 0

    # PRESSURE CORRECTION
    # pressure correction factor top-soil
    pct_front = np.log10(list(p_front)) - 1.2
    pct_rear = np.log10(list(p_rear)) - 1.2
    pct_machine = np.log10(list(p_machine)) - 1.2

    # pressure correction factor mid-soil
    pcm_front = np.log10(list(p_front)) - 0.53
    pcm_rear = np.log10(list(p_rear)) - 0.53
    pcm_machine = np.log10(list(p_machine)) - 0.53

    # pressure correction factor bottom-soil
    pcb_front = np.log10(list(p_front)) - 0.27
    pcb_rear = np.log10(list(p_rear)) - 0.27
    pcb_machine = np.log10(list(p_machine)) - 0.27

    # TKM-PCORR
    # tkm-pcorr top-soil
    tkm_pcorr_t_front = tkmt_front * pct_front
    tkm_pcorr_t_rear = tkmt_rear * pct_rear
    tkm_pcorr_t_machine = tkmt_machine * pct_machine

    # tkm-pcorr mid-soil
    tkm_pcorr_m_front = tkmm_front * pcm_front
    tkm_pcorr_m_rear = tkmm_rear * pcm_rear
    tkm_pcorr_m_machine = tkmm_machine * pcm_machine

    # tkm-pcorr bottom-soil
    tkm_pcorr_b_front = tkmb_front * pcb_front
    tkm_pcorr_b_rear = tkmb_rear * pcb_rear
    tkm_pcorr_b_machine = tkmb_machine * pcb_machine


    """ WITH furrow wheel """

    # WEIGHT CORRECTION
    # corrected weight top-soil
    cwt_front = w_front_full / 1000
    cwt_rear = w_rear_full / 1000

    # corrected weight mid-soil
    cwm_front = 0.5 * (w_front_full - 4000) / 1000 + 0.5 * w_front_full / 1000
    cwm_rear = 0.5 * (w_rear_full - 4000) / 1000 + 0.5 * w_rear_full / 1000
    # set negative values to 0
    cwm_front[cwm_front < 0] = 0
    cwm_rear[cwm_rear < 0] = 0

    # corrected weight bottom-soil
    cwb_front = 0.5 * (w_front_full - 6000) / 1000 + 0.5 * (w_front_full - 3000) / 1000
    cwb_rear = 0.5 * (w_rear_full - 6000) / 1000 + 0.5 * (w_rear_full - 3000) / 1000
    # set negative values to 0
    cwb_front[cwb_front < 0] = 0
    cwb_rear[cwb_rear < 0] = 0

    # TKM
    # tkm top-soil
    tkmt_front = distance * cwt_front
    tkmt_rear = distance * cwt_rear

    # tkm mid-soil
    tkmm_front = distance * cwm_front
    tkmm_rear = distance * cwm_rear

    # tkm bottom-soil
    tkmb_front = distance * cwb_front
    tkmb_rear = distance * cwb_rear

    # PRESSURE CORRECTION
    # pressure correction factor top-soil
    pct_front = np.log10(list(p_front)) - 1.2
    pct_rear = np.log10(list(p_rear)) - 1.2

    # pressure correction factor mid-soil
    pcm_front = np.log10(list(p_front)) - 0.53
    pcm_rear = np.log10(list(p_rear)) - 0.53

    # pressure correction factor bottom-soil
    pcb_front = np.log10(list(p_front)) - 0.27
    pcb_rear = np.log10(list(p_rear)) - 0.27

    # TKM-PCORR
    # tkm-pcorr top-soil
    tkm_plough_pcorr_t_front = tkmt_front * pct_front
    tkm_plough_pcorr_t_rear = tkmt_rear * pct_rear
    tkm_plough_pcorr_t_machine = np.zeros(len(tkm_plough_pcorr_t_front))  # array with zeros

    # tkm-pcorr mid-soil
    tkm_plough_pcorr_m_front = tkmm_front * pcm_front
    tkm_plough_pcorr_m_rear = tkmm_rear * pcm_rear
    tkm_plough_pcorr_m_machine = np.zeros(len(tkm_plough_pcorr_m_front))  # array with zeros

    # tkm-pcorr bottom-soil
    tkm_plough_pcorr_b_front = tkmb_front * pcb_front
    tkm_plough_pcorr_b_rear = tkmb_rear * pcb_rear
    tkm_plough_pcorr_b_machine = np.zeros(len(tkm_plough_pcorr_b_front))  # array with zeros


    # Sum up per layer
    tkm_pcorr_t = tkm_pcorr_t_front + tkm_pcorr_t_rear + tkm_pcorr_t_machine
    tkm_pcorr_m = tkm_pcorr_m_front + tkm_pcorr_m_rear + tkm_pcorr_m_machine
    tkm_pcorr_b = tkm_pcorr_b_front + tkm_pcorr_b_rear + tkm_pcorr_b_machine

    tkm_plough_pcorr_t = tkm_plough_pcorr_t_front + tkm_plough_pcorr_t_rear + tkm_plough_pcorr_t_machine
    tkm_plough_pcorr_m = tkm_plough_pcorr_m_front + tkm_plough_pcorr_m_rear + tkm_plough_pcorr_m_machine
    tkm_plough_pcorr_b = tkm_plough_pcorr_b_front + tkm_plough_pcorr_b_rear + tkm_plough_pcorr_b_machine


    # Merge machines without and with furrow wheel in one dataframe:

    # create dataframes
    index = ['tkm_per_pass top soil', 'tkm_per_pass mid soil', 'tkm_per_pass bottom soil', 'furrow_wheel', 'identfier']
    tkm_noplough_pcorr = pd.DataFrame(np.array([tkm_pcorr_t, tkm_pcorr_m, tkm_pcorr_b, data['furrow_wheel'],
                                                data['identifier']]),
                                      index, columns=data['identifier'])
    tkm_plough_pcorr = pd.DataFrame(np.array([tkm_plough_pcorr_t, tkm_plough_pcorr_m, tkm_plough_pcorr_b,
                                              data['furrow_wheel'], data['name']]),
                                    index, columns=data['identifier'])

    # select depending on whether there is a furrow wheel or not and put selection in one dataframe
    a = tkm_noplough_pcorr[[col for col in tkm_noplough_pcorr.columns
                            if (tkm_noplough_pcorr[col].loc['furrow_wheel'] == "no")]]
    b = tkm_plough_pcorr[[col for col in tkm_plough_pcorr.columns
                          if (tkm_plough_pcorr[col].loc['furrow_wheel'] == "yes")]]
    tkm_pcorr = pd.concat([a, b], axis=1).transpose()

    tkm_pcorr

    tkm = pd.merge(df, tkm_pcorr, how='right', on='identifier')

    return tkm


