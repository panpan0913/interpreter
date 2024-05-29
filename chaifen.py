import os
import shutil

def split_drc_file(filename):
    with open(filename, 'r') as file:
        lines = file.readlines()

    # 找到最后一个INPUT行的索引
    last_input_index = max(i for i, line in enumerate(lines) if 'INPUT' in line)

    # 将最后一个INPUT之前的所有行存储在header_lines中
    header_lines = lines[:last_input_index+1]

    output_lines = []
    output_name = None
    for line in lines[last_input_index+1:]:
        output_lines.append(line)
        if '=' in line:
            output_name = line.split('=')[1].split()[0]
        if 'OUTPUT' in line:
            # 为每个新的文件创建一个同名的子文件夹
            os.makedirs(output_name, exist_ok=True)
            with open(os.path.join(output_name, f'{output_name}.drc'), 'w') as output_file:
                output_file.writelines(header_lines + output_lines)
            # 复制并重命名top.gds文件
            shutil.copy('top.gds', os.path.join(output_name, f'{output_name}.gds'))
            output_lines = []
            output_name = None

    if output_lines:  # 如果最后还有一些没有输出的行
        os.makedirs(output_name, exist_ok=True)
        with open(os.path.join(output_name, f'{output_name}.drc'), 'w') as output_file:
            output_file.writelines(header_lines + output_lines)
        shutil.copy('top.gds', os.path.join(output_name, f'{output_name}.gds'))

split_drc_file('test1.drc')