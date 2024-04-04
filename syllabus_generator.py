import yaml


    

# from https://chat.openai.com/share/c89ed987-13d0-4ec7-966e-331ea519d4a7
def parse_md_table(md_table):
    """
    Parse a markdown table string into a list of dictionaries.
    
    :param md_table: Markdown table string
    :return: List of dictionaries representing the table
    """
    lines = md_table.strip().split('\n')

    # Extract headers
    headers_tmp = lines[0].strip().split('|')
    headers = [header.strip() for header in headers_tmp[1:len(headers_tmp)-1]]
    print(headers)
    # Extract rows
    rows = []
    for line in lines[2:]:
        row_tmp = [cell.strip() for cell in line.strip().split('|')]
        row = row_tmp[1:len(row_tmp)-1]
        print("Row")
        print(row)
        row_dict = dict(zip(headers, row))
        rows.append(row_dict)

    return rows

# modified from https://chat.openai.com/share/c89ed987-13d0-4ec7-966e-331ea519d4a7
def generate_md_table(data,headers=None):
    """
    Generate a markdown table from a list of dictionaries, padding each column
    so the pipes line up.

    :param data: List of dictionaries
    :return: Markdown table string
    """
    if not data:
        return ""

    # Get headers, if not provided
    if headers==None:
        headers = data[0].keys()

    # Determine maximum width for each column (based on both headers and data)
    column_widths = {}
    for header in headers:
        max_width = max([len(str(row.get(header, ""))) for row in data] + [len(header)])
        column_widths[header] = max_width

    # Create the header line with padding
    header_line = "| " + " | ".join([header.ljust(column_widths[header]) for header in headers]) + " |"

    # Create the divider with padding
    divider = "| " + " | ".join(["-" * column_widths[header] for header in headers]) + " |"

    # Create rows with padding
    rows = []
    for row in data:
        row_line = "| " + " | ".join([str(row[header]).ljust(column_widths[header]) for header in headers]) + " |"
        rows.append(row_line)

    return "\n".join([header_line, divider] + rows)


def getVal(dict,key):
    print("Dict: " + str(dict))
    if key in dict:
        tmp = dict[key]
        if tmp==None:
            return("")
        if type(tmp)==list:
            print("Found a list!")
            return("; ".join(tmp))
        else:
            return(tmp)
    else:
        return("")

with open('index.markdown','w') as outfile:
    with open('assets/syllabus/syllabus_top.markdown','r') as syllabus_top:
        outfile.write(syllabus_top.read())
    
    with open('assets/syllabus/syllabus_table.org','r') as order_file:
        syllabus_sketch = parse_md_table(order_file.read())
        print(syllabus_sketch)
        print(generate_md_table(syllabus_sketch))
        with open('class_sessions.yml','r') as file:
            lectures = yaml.safe_load(file)['lectures']

            print(lectures)
            print(type(lectures))

            new_rows = []
            for row in syllabus_sketch:
                print("Row: " + str(row))
                print(type(row))
                print(row.keys())
                print("Topic: " + str(row['Topic']))
                new_row = {'Week': row['Week of semester'],
                           'Day': row['Day'],
                           'Topic': getVal(lectures[row['Topic']],'topic'),
                           #'Videos': getVal(lectures[row['Topic']],'videos'),
                           'Slides': getVal(lectures[row['Topic']],'slides'),
                           'Readings': getVal(lectures[row['Topic']],'readings'),
                           #'In-class handouts and exercises': row['In-class materials'],
                           #'Optional videos': getVal(lectures[row['Topic']],'optional videos'),
                           'Related readings': getVal(lectures[row['Topic']],'related readings'),
                           'Problem sets': row['Psets']}
                new_rows.append(new_row)
            print(new_rows)
            outfile.write(generate_md_table(new_rows))
    outfile.close()

            



if __name__ == '__main__':
    # Example markdown table
    md_table = """

    | Alice  | 28  | Engineer    |
    | Bob    | 24  | Data Scientist |
    | Charlie| 35  | Doctor      |
    """

    print(md_table)
    data = parse_md_table(md_table)
    for row in data:
        print(row)

    print(generate_md_table(data))


    
