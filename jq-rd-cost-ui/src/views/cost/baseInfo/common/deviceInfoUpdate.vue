<template>
  <div>
    <el-form ref="detail" :model="detail" :rules="rules" label-width="100px" :disabled="disabled">
      <!-- 基本信息 -->
      <jq-panel title="基本信息">
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
            <el-form-item label="客户名称" prop="companyId">
              <jq-select-company :companyId.sync="detail.companyId"/>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="8">
            <el-form-item label="设备类型" prop="deviceType">
              <el-select v-model="detail.deviceType" placeholder="请选择设备类型" class="form-control" clearable filterable>
                <el-option
                  v-for="dict in deviceTypeOptions"
                  :key="dict.dictValue"
                  :label="dict.dictLabel"
                  :value="dict.dictValue"
                ></el-option>
              </el-select>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="设备用途" prop="deviceUse">
              <el-select v-model="detail.deviceUse" placeholder="请选择设备用途" class="form-control" clearable filterable>
                <el-option
                  v-for="dict in deviceUseOptions"
                  :key="dict.dictValue"
                  :label="dict.dictLabel"
                  :value="dict.dictValue"
                ></el-option>
              </el-select>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="规格型号" prop="deviceSpecs">
              <el-input v-model="detail.deviceSpecs" placeholder="请输入规格型号"/>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="8">
            <el-form-item label="取得日期" prop="getDate">
              <el-date-picker
                clearable size="small"
                v-model="detail.getDate"
                class="form-control"
                type="date"
                value-format="yyyy-MM-dd"
                placeholder="选择取得日期"
              >
              </el-date-picker>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="使用部门" prop="getDeptId">
              <treeselect v-model="detail.getDeptId" :options="getDeptIdOptions" placeholder="请选择" class="form-control"/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="使用人" prop="getUserId">
              <jq-user-select :check-many="false" :agent.sync="detail.getUserId" placeholder="请选择使用人"/>
            </el-form-item>
          </el-col>
        </el-row>
      </jq-panel>

      <!-- 产能和折旧 -->
      <jq-panel title="产能和折旧">
        <el-row>
          <el-col :span="8">
            <el-form-item label="资产原值" prop="cost">
              <el-input v-model="detail.cost" placeholder="请输入资产原值" @change="calculateMonthlyAmount"/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="净残值率(%)" prop="residualRate">
              <el-input v-model.number="detail.residualRate" placeholder="请输入净残值率" @change="calculateMonthlyAmount"/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="可用月数" prop="usableMonths">
              <el-input v-model="detail.usableMonths" placeholder="请输入可用月数" @change="calculateMonthlyAmount"/>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="8">
            <el-form-item label="已折月数" prop="convertedMonths">
              <el-input v-model="detail.convertedMonths" placeholder="请输入已折月数"/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="月折旧额" prop="monthlyAmount">
              <el-input v-model="detail.monthlyAmount" placeholder="请输入月折旧额" disabled/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="日产能" prop="dailyCapacity">
              <el-input v-model="detail.dailyCapacity" placeholder="请输入日产能"/>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="8">
            <el-form-item label="日有效机时" prop="monthlyEffectiveHours">
              <el-input v-model="detail.monthlyEffectiveHours" placeholder="请输入日有效机时"/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="额度功率kwh" prop="ratedPower">
              <el-input v-model="detail.ratedPower" placeholder="请输入额度功率"/>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="24">
            <el-form-item label="备注" prop="remark">
              <el-input type="textarea" rows="10" v-model="detail.remark" placeholder="请输入备注"/>
            </el-form-item>
          </el-col>
        </el-row>
        <div class="fixed_coperate" v-show="!disabled">
          <el-button type="primary" @click="submitForm">确 定</el-button>
          <el-button @click="handleClose">取 消</el-button>
        </div>
      </jq-panel>
    </el-form>
  </div>
</template>

<script>
import { isNullOrEmpty } from '@/utils/jq'
import { getDeptNameInfo } from '@/api/cost/deptInfo'
import { getWorkingInfoByCompanyIdAndItemNo } from '@/api/cost/workinginfo'
import { addDeviceInfo, getDeviceInfo, updateDeviceInfo } from '@/api/cost/deviceInfo'

export default {
  name: 'deviceInfoUpdate',
  components: {},
  inject: ['handleQuery'],
  data() {
    return {
      // 设备类型字典
      deviceTypeOptions: [],
      // 设备用途字典
      deviceUseOptions: [],
      // 部门下拉框
      getDeptIdOptions: [],
      //表单信息
      detail: {},
      //表单校验
      rules: {
        deviceName: [
          { required: true, message: '设备名称不能为空', trigger: 'blur' }
        ],
        deviceType: [
          { required: true, message: '设备类型不能为空', trigger: 'change' }
        ],
        deviceUse: [
          { required: true, message: '用途不能为空', trigger: 'blur' }
        ],
        getDate: [
          { required: true, message: '取得日期不能为空', trigger: 'blur' }
        ],
        getDeptId: [
          { required: true, message: '使用部门不能为空', trigger: 'blur' }
        ],
        usableMonths: [
          { required: true, message: '可用月数不能为空', trigger: 'blur' }
        ],
        cost: [
          { required: true, message: '资产原值不能为空', trigger: 'blur' }
        ],
        monthlyEffectiveHours: [
          { required: true, message: '日有效机时不能为空', trigger: 'blur' },
          {
          validator: (rule, value, callback) => {
           if (value > 24) {
             callback(new Error('日有效机时不能大于24'));
         } else {
           callback();
         }
        }
     }
        ],
        monthlyAmount: [
          { required: true, message: '月折旧额不能为空', trigger: 'blur' }
        ],
        residualRate: [
          { required: true, message: '净残值率不能为空', trigger: 'blur' },
          { type: 'number', min: 0, max: 100, message: '净残值率在 0 到 100 之间' }
        ]
      }
    }
  },
  props: {
    deviceInfoId: {
      type: Number
    },
    disabled: {
      type: Boolean,
      require: true
    }
  },
  watch: {
    deviceInfoId(val) {
      if (isNullOrEmpty(val)) {
        this.reset()
      } else {
        this.getDetail(val)
      }
    }
  },
  created() {
    //设备类型字典
    this.getDicts('device_type').then(response => {
      this.deviceTypeOptions = response.data
    })
    //设备用途字典
    this.getDicts('device_use').then(response => {
      this.deviceUseOptions = response.data
    })
    /** 查询使用部门下拉树结构 */
    getDeptNameInfo(this.$store.state.item.companyId, this.$store.state.item.itemNo).then(response => {
      this.getDeptIdOptions = response.data
    })

    this.getDetail(this.deviceInfoId)
  },
  methods: {
    /** 表单重置 */
    reset() {
      this.detail = {
        //附件列表
        attachmentList: [],
        id: null,
        deviceNo: null,
        deviceName: null,
        deviceType: null,
        deviceUse: null,
        deviceSpecs: null,
        getDate: null,
        getDeptId: null,
        getUserId: null,
        usableMonths: null,
        cost: null,
        convertedMonths: null,
        residualRate: 0,
        monthlyAmount: null,
        dailyCapacity: null,
        monthlyEffectiveHours: null,
        ratedPower: null,
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
      this.resetForm('detail')
    },

    /** 获取企业设备信息详情 */
    getDetail(deviceInfoId) {
      if (isNullOrEmpty(deviceInfoId)) {
        this.reset()
        getWorkingInfoByCompanyIdAndItemNo(this.$store.state.item.companyId, this.$store.state.item.itemNo).then(response => {
          if (!isNullOrEmpty(response.data)) {
            this.detail.monthlyEffectiveHours = response.data.workingHour
          }
        })
      } else {
        getDeviceInfo(deviceInfoId).then(response => {
          this.detail = response.data
        })
      }
    },

    /** 计算月折旧额 */
    //月折旧额（元） = 资产原值*（1-净残值率）/可用月数
    calculateMonthlyAmount() {
      if (!isNullOrEmpty(this.detail.cost) && !isNullOrEmpty(this.detail.residualRate) && !isNullOrEmpty(this.detail.usableMonths)) {
        this.detail.monthlyAmount = this.detail.cost * (100 - this.detail.residualRate) / this.detail.usableMonths / 100
      } else {
        this.detail.monthlyAmount = 0
      }
    },

    /** 提交按钮 */
    submitForm() {
      this.$refs['detail'].validate(valid => {
        if (valid) {
          if (this.detail.id != null) {
            updateDeviceInfo(this.detail).then(response => {
              if (response.code === 200) {
                this.msgSuccess(response.msg)
                this.handleClose()
                this.handleQuery()
              }
            })
          } else {
            this.detail.itemNo = this.$store.state.item.itemNo
            addDeviceInfo(this.detail).then(response => {
              if (response.code === 200) {
                this.msgSuccess(response.msg)
                this.handleClose()
                this.handleQuery()
              }
            })
          }
        }
      })
    },

    /** 关闭按钮 */
    handleClose() {
      this.$emit('handleClose')
    }

  }
}
</script>
