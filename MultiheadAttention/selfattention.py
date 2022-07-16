# -*- coding: utf-8 -*-
"""
Created on Fri Jul 15 01:03:38 2022

@author: 21916
"""


import numpy as np
import torch
from torch import nn
import pandas as pd
from torch.autograd import Variable
#读取转化数据--------------------------------------------------------------------
data = pd.read_excel(r"D:\Desktop\fd.xlsx",index_col=None)
dataset = np.array(data,dtype="float32")

#创建训练集和测试集---------------------------------------------------------------
data_len=int(dataset[:,0].size)
train_data_ratio = 0.8 # Choose 80% of the data for testing
train_data_len = int(data_len*train_data_ratio)
train_x = dataset[:train_data_len, 0:5]
train_y = dataset[:train_data_len, 5]
test_x = dataset[train_data_len:, 0:5]
test_y = dataset[train_data_len:, 5]

#LSTM模型定义-------------------------------------------------------------------
class LstmRNN(nn.Module):
    """
        Parameters：
        - input_size: feature size
        - hidden_size: number of hidden units
        - output_size: number of output
    """
    def __init__(self, input_size, hidden_size=20, output_size=1,num_heads=1):
        super().__init__()
 
        self.MHattention = nn.MultiheadAttention(1, num_heads) # utilize the MultiheadAttention model in torch.nn 
        self.forwardCalculation1 = nn.Linear(input_size, hidden_size)
        self.forwardCalculation = nn.Linear(hidden_size, output_size)
 
    def forward(self, _x):
        x = self.MHattention(_x.T,_x.T,_x.T)[0] # _x is input, size (seq_len, batch, input_size)
        x = self.forwardCalculation1(x.T)
        s, b, h = x.shape  # x is output, size (seq_len, batch, hidden_size)
        x = x.view(s*b, h)
        x = self.forwardCalculation(x)
        x = x.view(s, b, -1)
        return x
    
#检查显卡是否可用----------------------------------------------------------------
device = torch.device("cpu")
if (torch.cuda.is_available()):
    device = torch.device("cuda:0")
    print('Training on GPU.')
else:
    print('No GPU available, training on CPU.')

#网络参数设置--------------------------------------------------------------------
INPUT_FEATURES_NUM = 5
OUTPUT_FEATURES_NUM = 1
train_x_tensor = train_x.reshape(-1, 800, INPUT_FEATURES_NUM) # set batch 大小应设置为处理数据的重复次数量，建议值为1
train_y_tensor = train_y.reshape(-1, 800, OUTPUT_FEATURES_NUM) 
# transfer data to pytorch tensor----------------------------------------------
train_x_tensor = torch.from_numpy(train_x_tensor)
train_y_tensor = torch.from_numpy(train_y_tensor)
train_x_tensor = train_x_tensor.to(device)
train_y_tensor = train_y_tensor.to(device)

#实例化-------------------------------------------------------------------------
MHattention_model = MHattentionRNN(INPUT_FEATURES_NUM, 20, output_size=OUTPUT_FEATURES_NUM, num_layers=2).to(device)  # 20 hidden units
print('MultiheadAttention model:', MHattention_model)
print('model.parameters:', MHattention_model.parameters)
print('train x tensor dimension:', Variable(train_x_tensor).size()) 
loss_function = nn.MSELoss().to(device)
optimizer = torch.optim.Adam(MHattention_model.parameters(), lr=1e-3)

#开始训练-----------------------------------------------------------------------
prev_loss = 0.1
max_epochs = 10000#最大循环次数
for epoch in range(max_epochs):
    output = MHattention_model(train_x_tensor).to(device)
    loss = loss_function(output, train_y_tensor).to(device) 
    loss.backward()
    optimizer.step()
    optimizer.zero_grad()
    if loss.item() < 1e-5:
            print('Epoch [{}/{}], Loss: {:.5f}'.format(epoch+1, max_epochs, loss.item()))
            print("The loss value is reached")
            break
    elif (epoch+1) % 100 == 0:
            print('Epoch: [{}/{}], Loss:{:.5f}'.format(epoch+1, max_epochs, loss.item()))
#------------------------------------------------------------------------------            
#需要输出模型选下面的    
"""
    if loss < prev_loss:
        torch.save(MHattention_model.state_dict(), 'MHattention_model.pt')  # save model parameters to files      
        prev_loss = loss 
    if loss.item() < 1e-4:
        print('Epoch [{}/{}], Loss: {:.5f}'.format(epoch + 1, max_epochs, loss.item()))
        print("The loss value is reached")
        break
    elif (epoch + 1) % 100 == 0:
        print('Epoch: [{}/{}], Loss:{:.5f}'.format(epoch + 1, max_epochs, loss.item()))
"""
#训练集预测----------------------------------------------------------------------
predictive_y_for_training = MHattention_model(train_x_tensor).cpu()
predictive_y_for_training = predictive_y_for_training.view(-1, OUTPUT_FEATURES_NUM).data.numpy().flatten()
result_train=pd.DataFrame({"pre":predictive_y_for_training,"obs":train_y})
result_train.to_excel("D:\\Desktop\\result_train.xlsx")

#测试集预测----------------------------------------------------------------------
MHattention_model = MHattention_model.eval() # switch to testing model
# prediction on test dataset
test_x_tensor = test_x.reshape(-1,200, INPUT_FEATURES_NUM) # set batch size to 5, the same value with the training set
test_x_tensor = torch.from_numpy(test_x_tensor).to(device)
predictive_y_for_testing = MHattention_model(test_x_tensor).cpu()
predictive_y_for_testing = predictive_y_for_testing.view(-1, OUTPUT_FEATURES_NUM).data.numpy().flatten()
result3=pd.DataFrame({"pre":predictive_y_for_testing,"obs":test_y})
result3.to_excel("D:\\Desktop\\result_testr.xlsx")


 

