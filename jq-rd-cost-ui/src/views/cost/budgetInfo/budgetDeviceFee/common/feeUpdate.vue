<template>
  <div>
    <el-form ref="detail" :model="detail" :rules="rules" label-width="100px" :disabled="disabled">
      <jq-panel title="设备基本信息">
        <el-row>
          <el-col :span="8">
            <el-form-item label="设备名称" prop="deviceName">
              <el-input v-model="detail.deviceName" placeholder="请输入设备名称"/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="设备编码" prop="deviceNo">
              <el-input v-model="detail.deviceNo" placeholder="自动生成" disabled/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="设备类型" prop="deviceType">
              <el-select v-model="detail.deviceType" placeholder="请选择设备类型" class="form-control" clearable
                         filterable>
                <el-option
                  v-for="dict in deviceTypeOptions"
                  :key="dict.dictValue"
                  :label="dict.dictLabel"
                  :value="dict.dictValue"
                ></el-option>
              </el-select>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="8">
            <el-form-item label="可用月数" prop="usableMonths">
              <el-input v-model="detail.usableMonths" placeholder="请输入可用月数" />
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="月折旧额" prop="monthlyAmount">
              <el-input v-model="detail.monthlyAmount" placeholder="请输入月折旧额"/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="资产原值" prop="cost">
              <el-input v-model="detail.cost" placeholder="请输入资产原值" />
            </el-form-item>
          </el-col>
        </el-row>
      </jq-panel>

      <jq-panel title="设备折旧明细">
        <el-table :data="detail.budgetDeviceFees" style="margin-bottom: 22px;" :rules="[]">
          <el-table-column label="月度" :sortable="false" prop="month">
          </el-table-column>
          <el-table-column label="月有效机时" :sortable="false" prop="monthlyEffectiveHours">
          </el-table-column>
          <el-table-column prop="devHours" label="设备投入研发工时">
            <template slot-scope="scope">
              <el-input v-model="scope.row.devHours" @input="val =>handleInput(val,scope.row)"></el-input>
            </template>
          </el-table-column>
          <el-table-column prop="devFee" label="设备折旧摊销费用">
            <template slot-scope="scope">

              <el-form-item :prop="'budgetDeviceFees.' + scope.$index + '.devFee'" :rules='rules.devFee'>
                <el-input readonly v-model="scope.row.devFee"></el-input>
              </el-form-item>
            </template>
          </el-table-column>
        </el-table>
      </jq-panel>
      <div class="fixed_coperate" v-show="!disabled">
        <el-button type="primary" @click="submitForm" :loading="loading">确 定</el-button>
        <el-button @click="cancel">取 消</el-button>
      </div>
    </el-form>
  </div>
</template>

<script>
  import {isNullOrEmpty} from '@/utils/jq'
  import {getBaseDeviceInfo, delFee, addFee, updateByDeviceInfo,getStatistics} from '@/api/cost/budgetDeviceFee.js';
  import {Message} from 'element-ui'
  export default {
    name: 'feeUpdate',
    components: {},
    inject: ['handleQuery'],
    data() {
      return {
        detail: {},
        //表单校验
        rules:{
          devFee:[{ required: true, validator: (rule, value, callback) => {
              console.log('validator',rule, value, callback)
              let sumDevFee = this.detail.budgetDeviceFees
                                .map(elt=>Number(elt.devFee))
                                .reduce((prev, curr) => prev + curr, 0);
              if (sumDevFee>this.statistics.totalEquipmentFee) {
                  Message.error('当前设备折旧摊销费用总计:'+sumDevFee+',不能大于期望折旧费用:'+this.statistics.totalInvisibleFee)
                  return callback(new Error('err'));
              }
              return callback();
            }, trigger: "change" }]
          },
        deviceTypeOptions:[],
        statistics:{},
        itemNo:this.$store.state.item.itemNo,
        companyId:this.$store.state.item.companyId,
        loading:false
      }
    },
    props: {
      feeId: {
        type: Number,
      },
      disabled: {
        type: Boolean,
        require: true
      }
    },
    watch: {
      feeId(val) {
        if (isNullOrEmpty(val)) {
          this.reset()
        } else {
          this.getDetail(val)
        }
      }
    },
    created() {
      this.getDetail(this.feeId);
      this.getDicts('device_type').then(response => {
        this.deviceTypeOptions = response.data
      })
      this.budgetDeviceFeeGetStatistics();
    },
    methods: {
      /** 表单重置 */
      reset() {
        this.detail = {
          //附件列表
          attachmentList: [],
          id: null,
          deviceId: null,
          month: null,
          devHours: null,
          devFee: null,
          itemNo: null,
          companyId: null,
          tenantId: null,
          revision: null,
          createBy: null,
          createTime: null,
          updateBy: null,
          updateTime: null,
          remark: null,
          delFlag: null
        }
        this.resetForm("detail");
      },

      /** 获取研发投入设备折旧费用拟定;详情 */
      getDetail(feeId) {
        if (isNullOrEmpty(feeId)) {
          this.reset()
        } else {
          getBaseDeviceInfo(feeId).then(response => {
            this.detail = response.data
          });
        }
      },

      /** 提交按钮 */
      submitForm() {
        let self = this;
        this.$refs["detail"].validate(valid => {
          if (valid) {
            if (this.detail.id != null) {
              self.loading=true;
              updateByDeviceInfo({budgetDeviceFees:this.detail.budgetDeviceFees}).then(response => {
                self.loading=false;
                if (response.code === 200) {
                  this.msgSuccess(response.msg)
                  this.handleClose()
                  this.handleQuery()
                }
              });
            }
          }
        });
      },

      /** 取消按钮 */
      cancel() {
        if (this.feeId) {
          this.getDetail(this.feeId)
          this.$emit('update:disabled', true)
        }
      },

      /** 关闭按钮 */
      handleClose() {
        this.$emit('handleClose')
      },
      handleInput(val,row){
        console.log(val,row)
        row.devHours = val.replace(/^([0-9]\d*\.?\d{0,2})?.*$/,'$1')
        if (row.devHours&&row.monthlyEffectiveHours) {
          // row.devFee=this.detail.monthlyAmount/row.devHours*row.monthlyEffectiveHours
          row.devFee=row.devHours/row.monthlyEffectiveHours*this.detail.monthlyAmount
        }
      },
      budgetDeviceFeeGetStatistics() {
        getStatistics({itemNo:this.itemNo,companyId:this.companyId}).then(resp=>{
          this.statistics=resp.data;
        })

      }
    }
  }
</script>
