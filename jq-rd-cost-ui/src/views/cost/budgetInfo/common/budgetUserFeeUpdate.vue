<template>
  <div>
    <el-form ref="detail" :model="detail" :rules="rules" label-width="100px" :disabled="disabled">
      <jq-panel title="员工基本信息">
        <el-row>
          <el-col :span="8">
            <el-form-item label="员工姓名" prop="name">
              <el-input v-model="detail.name" placeholder="请输入员工姓名" disabled/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="学历" prop="education">
              <el-select v-model="detail.education" placeholder="请选择学历" class="form-control" clearable
                         filterable disabled>
                <el-option
                  v-for="dict in educationOptions"
                  :key="dict.dictValue"
                  :label="dict.dictLabel"
                  :value="dict.dictValue"
                ></el-option>
              </el-select>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="所属部门" prop="deptId">
              <treeselect v-model="detail.deptId" placeholder="请输入所属部门" :multiple="false" :options="deptIdOptions"  class="form-control"
                          :disabled="disabled" disabled/>
            </el-form-item>
          </el-col>
        </el-row>
        <el-row>
          <el-col :span="8">
            <el-form-item label="人员类别" prop="userType">
              <el-select v-model="detail.userType" placeholder="请选择人员类别" class="form-control" clearable
                         filterable>
                <el-option
                  v-for="dict in userTypeOptions"
                  :key="dict.dictValue"
                  :label="dict.dictLabel"
                  :value="dict.dictValue"
                ></el-option>
              </el-select>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="职务" prop="duties">
              <el-input v-model="detail.duties" placeholder="请输入职务" disabled/>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="聘用方式" prop="employMethod">
              <el-select v-model="detail.employMethod" placeholder="请选择聘用方式" class="form-control" clearable
                         filterable disabled>
                <el-option
                  v-for="dict in employMethodOptions"
                  :key="dict.dictValue"
                  :label="dict.dictLabel"
                  :value="dict.dictValue"
                ></el-option>
              </el-select>
            </el-form-item>
          </el-col>
        </el-row>
      </jq-panel>
      <jq-panel title="投入研发工时明细">
        <el-table :data="detail.salaryDetailList" style="margin-bottom: 22px;">
          <el-table-column prop="salaryMonth" label="月度">
          </el-table-column>
          <el-table-column prop="salaryFee" label="应发工资">
          </el-table-column>
          <el-table-column prop="safeFee" label="社保（企业）">
          </el-table-column>
          <el-table-column prop="goldFee" label="公积金（企业）">
          </el-table-column>
          <el-table-column prop="workingDate" label="当月出勤天数">
          </el-table-column>
          <el-table-column prop="sumWorkingHour" label="当月总工时">
          </el-table-column>
          <el-table-column label="当月投入研发工时" :sortable="false">
            <template slot-scope="scope">
              <el-input v-model="scope.row.devHours" placeholder="当月投入研发工时" @input="updateDevSalaryFee(scope.row)"/>
            </template>
          </el-table-column>
          <el-table-column prop="devSalaryFee" label="当月投入人员费用（工资）">
          </el-table-column>
          <el-table-column prop="devSocialFee" label="当月投入人员费用（社保）">
          </el-table-column>
          <el-table-column prop="devProvidentFee" label="当月投入人员费用（公积金）">
          </el-table-column>
        </el-table>
      </jq-panel>
      <div class="fixed_coperate" v-show="!disabled">
        <el-button type="primary" @click="submitForm">确 定</el-button>
        <el-button @click="cancel">取 消</el-button>
      </div>
    </el-form>
  </div>
</template>

<script>
  import { isNullOrEmpty } from '@/utils/jq'
  import {
    getBudgetUserFee,
    delBudgetUserFee,
    addBudgetUserFee,
    updateBudgetUserFee,
    selectUserFee, getBaseUserInfo, updateByUserInfo
  } from '@/api/cost/budgetUserFee'
  import { getDeptNameInfo } from '@/api/cost/deptInfo'
  import { getUserInfo } from '@/api/cost/userInfo'

  export default {
    name: 'budgetUserFeeUpdate',
    components: {},
    inject: ['handleQuery'],
    data() {
      return {
        userFeeList: [],
        // 聘用方式字典
        employMethodOptions: [],
        // 学历字典
        educationOptions: [],
        // 职务字典
        titleOptions: [],
        // 人员类别用途字典
        userTypeOptions: [],
        // 部门下拉框
        deptIdOptions: [],
        detail: {},
        //表单校验
        rules: {
          baseUserId: [
            { required: true, message: '员工ID不能为空', trigger: 'blur' }
          ], month: [
            { required: true, message: '月度;YYYY-MM不能为空', trigger: 'blur' }
          ]
        }
      }
    },
    props: {
      userInfoId: {
        type: Number
      },
      disabled: {
        type: Boolean,
        require: true
      }
    },
    watch: {
      userInfoId(val) {
        if (isNullOrEmpty(val)) {
          this.reset()
        } else {
          this.getDetail(val)
        }
      }
    },
    created() {
      this.getDetail(this.userInfoId)
      //聘用方式字典
      this.getDicts('engage_mode').then(response => {
        this.employMethodOptions = response.data
      })
      //学历字典
      this.getDicts('education').then(response => {
        this.educationOptions = response.data
      })
      //职务字典
      this.getDicts('position').then(response => {
        this.titleOptions = response.data
      })
      //人员类别字典
      this.getDicts('base_user_type').then(response => {
        this.userTypeOptions = response.data
      })
      let companyId = this.$store.state.item.companyId
      let itemNo = this.$store.state.item.itemNo
      this.getDeptNameInfo1(companyId, itemNo)
    },
    methods: {
      /** 表单重置 */
      reset() {
        this.detail = {
          //附件列表
          attachmentList: [],
          id: null,
          deptId: null,
          baseUserId: null,
          month: null,
          devHours: null,
          devSalaryFee: null,
          devSocialFee: null,
          devProvidentFee: null,
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
      /**输入自动计算当月投入人员费用(工资) */
      updateDevSalaryFee(detail) {
        // 在这里根据具体逻辑重新计算和更新 devSalaryFee
        const devHours = detail.devHours
        const sumWorkingHour = detail.sumWorkingHour
        const salaryFee = detail.salaryFee

        if (devHours && sumWorkingHour && salaryFee) {
          const devSalaryFee = (devHours / sumWorkingHour) * salaryFee
          detail.devSalaryFee = devSalaryFee
          this.detail.salaryDetailList.forEach(item => {
            if (item.feeId === detail.feeId) {
              item.devWages = detail.devSalaryFee
            }
          })

        }

      },

      /** 查询部门下拉树结构 */
      getDeptNameInfo1(companyId, itemNo) {
        getDeptNameInfo(companyId, itemNo).then(response => {
          this.deptIdOptions = response.data
        })
      },
      /** 获取人员费用拟定;详情 */
      getDetail(userInfoId) {
        if (isNullOrEmpty(userInfoId)) {
          this.reset()
        } else {
          getUserInfo(userInfoId).then(response => {
            this.detail = response.data
          })
        }
      },

      /** 提交按钮 */
      submitForm() {
        this.$refs['detail'].validate(valid => {
          if (valid) {
            if (this.detail.id != null) {
              updateByUserInfo(this.detail).then(response => {
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

      /** 取消按钮 */
      cancel() {
        if (this.userInfoId) {
          // this.getDetail(this.userInfoId)
          this.$emit('update:disabled', true)
        }
      },

      /** 关闭按钮 */
      handleClose() {
        this.$emit('handleClose')
      }

    }
  }
</script>
