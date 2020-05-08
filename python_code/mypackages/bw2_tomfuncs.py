import pandas as pd


def activity_list_to_dataframe(act_list, db):
    
    """
    This function returns a dataframe with name, location, unit, database,
    identifier and key of activities.
    Input: list of activities
    """

    name = []
    reference_product = []
    location = []
    unit = []
    classifications = []
    classification_isic = []
    classification_ecospold = []
    database = []
    identifier = []
    key = []


    for act in act_list:
        activity = db.get(act[1])
        name.append(activity['name'])
        reference_product.append(activity['reference product'])
        location.append(activity['location'])
        unit.append(activity['unit'])
        classifications.append(activity['classifications'])
        if 'ISIC' in str(activity['classifications'][0]):
            ci = activity['classifications'][0]
        elif len(activity['classifications']) > 1 and 'ISIC' in str(activity['classifications'][1]):
            ci = activity['classifications'][1]
        else:
            ci = ''
        classification_isic.append(ci)
        if 'EcoSpold' in str(activity['classifications'][0]):
            ce = activity['classifications'][0]
        elif len(activity['classifications']) > 1 and 'EcoSpold' in str(activity['classifications'][1]):
            ce = activity['classifications'][1]
        else:
            ce = ''
        classification_ecospold.append(ce)
        database.append(activity.key[0])
        identifier.append(activity.key[1])
        key.append(activity.key)
        

    # Create dataframe
    dictionary = {'name': name,
                  'reference_product': reference_product,
                  'location': location,
                  'unit': unit,
                  'classifications': classifications,
                  'classification_isic': classification_isic,
                  'classification_ecospold': classification_ecospold,
                  'key': key,
                  'database': database,
                  'identifier': identifier
                 }

    df = pd.DataFrame(dictionary, columns=['name', 'reference_product', 'location', 'unit',
                                           'classifications', 'classification_isic', 'classification_ecospold',
                                           'database', 'identifier', 'key'])
    df = df.sort_values(['name', 'location'])
    
    return df
    
    
def get_ecoinvent_activities(db, classification_list, omit_list):
    
    """
    This function returns a dataframe with agricultural machinery.
    Input: database, list of activity classifications
    """

    act_list = []  
    
    for c in classification_list:

        l = [i for i in db if c in str(i['classifications'])]

        for i in l:
            act_list.append(i)
            
    # Get rid of activities not relevant for compaction 
    for name in omit_list:
        act_list = [i for i in act_list if name not in str(i)]
    
    df = activity_list_to_dataframe(act_list, db)
    
    return df


def get_ecoinvent_market_activities(db, classification_list, omit_list):
    
    """
    This function returns a dataframe with agricultural machinery.
    Input: database, list of activity classifications
    """

    act_list = []  
    
    for c in classification_list:

        l = [i for i in db if c in str(i['classifications'])]

        for i in l:
            act_list.append(i)
            
    # Get rid of activities not relevant for compaction 
    for name in omit_list:
        act_list = [i for i in act_list if name not in str(i)]
    
    act_list = [i for i in act_list if 'market' in str(i)]
    
    df = activity_list_to_dataframe(act_list, db)
    
    return df


#def get_activities_contributions(activities_series, lca, db):
    
    """
    This function returns a dataframe with the activity contributions.
    Input: pandas Series of process identifier, lca object, database
    """

    """identifiers = []
    names = []
    locations = []
    units = []
    positions = []
    amounts = []
    for i in activities_series:
        activity = db.search(i)[0]
        position = lca.activity_dict[activity.key]
        amount = lca.supply_array[position]
        identifiers.append(i)
        names.append(activity['name'])
        locations.append(activity['location'])
        units.append(activity['unit'])
        amounts.append(amount)

    df = pd.DataFrame({'activity': names, 'location': locations, 'amount': amounts,
                                         'unit': units, 'identifier': identifiers})

    return df"""


def get_erosion_compaction_contributions(activities_series, lca, db_eco, db_soil):
    
    """
    This function returns a dataframe with the activity contributions.
    Input: pandas Series of process identifier, lca object, database
    """

    identifiers = []
    names = []
    locations = []
    units = []
    positions = []
    amounts = []
    for i in activities_series:
    
        if db_soil.search(i) != []:
            activity = db_soil.search(i)[0]
        elif db_eco.search(i) != []: 
            activity = db_eco.search(i)[0]
        else:
            continue
        position = lca.activity_dict[activity.key]
        amount = lca.supply_array[position]
        identifiers.append(i)
        names.append(activity['name'])
        locations.append(activity['location'])
        units.append(activity['unit'])
        amounts.append(amount)

    df = pd.DataFrame({'activity': names, 'location': locations, 'amount': amounts,
                                         'unit': units, 'identifier': identifiers})

    return df

