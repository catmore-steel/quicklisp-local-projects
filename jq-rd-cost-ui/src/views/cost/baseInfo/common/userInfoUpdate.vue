<template>
  <div>
    <el-form ref="detail" :model="detail" :rules="rules" label-width="100px" :disabled="disabled">
      <el-row>
        <el-col :span="8">
          <el-form-item label="姓名" prop="name">
            <el-input v-model="detail.name" placeholder="请输入姓名"/>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="工号" prop="jobNumber">
            <el-input v-model="detail.jobNumber" placeholder="请输入工号"/>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="性别" prop="sex">
            <el-select v-model="detail.sex" placeholder="请选择性别" class="form-control" clearable filterable>
              <el-option
                v-for="dict in sexOptions"
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
          <el-form-item label="人员类别" prop="userType">
            <el-select v-model="detail.userType" placeholder="请选择人员类别" class="form-control" clearable filterable>
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
          <el-form-item label="所属部门" prop="deptId">
            <treeselect v-model="detail.deptId" placeholder="请输入所属部门" :multiple="false"
                        :options="deptIdOptions" class="form-control"
            />
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="费用归集部门" prop="feeDeptId">
            <treeselect v-model="detail.feeDeptId" placeholder="请输入费用归集部门" :multiple="false"
                        :options="feeDeptIdOptions" class="form-control"
            />
          </el-form-item>
        </el-col>
      </el-row>
      <el-row>
        <el-col :span="8">
          <el-form-item label="职务" prop="duties">
            <el-input v-model="detail.duties" placeholder="请输入职务"/>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="学历" prop="education">
            <el-select v-model="detail.education" placeholder="请选择学历" class="form-control" clearable filterable>
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
          <el-form-item label="聘用方式" prop="employMethod">
            <el-select v-model="detail.employMethod" placeholder="请选择聘用方式" class="form-control" clearable filterable>
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
      <el-row>
        <el-col :span="8">
          <el-form-item label="职称" prop="title">
            <el-select v-model="detail.title" placeholder="请选择职称" class="form-control" clearable filterable>
              <el-option
                v-for="dict in titleOptions"
                :key="dict.dictValue"
                :label="dict.dictLabel"
                :value="dict.dictValue"
              ></el-option>
            </el-select>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="专业" prop="speciality">
            <el-input v-model="detail.speciality" placeholder="请输入专业"/>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="月平均工资" prop="averageWage">
            <el-input v-model="detail.averageWage" placeholder="请输入月平均工资"/>
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
    </el-form>
  </div>
</template>

<script>
import { isNullOrEmpty } from '@/utils/jq'
import { getDeptNameInfo } from '@/api/cost/deptInfo'
import { addUserInfo, getUserInfo, updateUserInfo } from '@/api/cost/userInfo'

export default {
  name: 'userInfoUpdate',
  components: {},
  inject: ['handleQuery'],
  data() {
    return {
      // 聘用方式字典
      employMethodOptions: [],
      // 学历字典
      educationOptions: [],
      // 职务字典
      titleOptions: [],
      // 人员类别用途字典
      userTypeOptions: [],
      // 性别用途字典
      sexOptions: [],
      // 部门下拉框
      deptIdOptions: [],
      //费用归集部门
      feeDeptIdOptions: [],
      detail: {},
      //表单校验
      rules: {
        name: [
          { required: true, message: '姓名不能为空', trigger: 'blur' },
          { type: 'string', message: '姓名不能存在数字' }
        ],
        deptId: [
          { required: true, message: '所属部门不能为空', trigger: 'blur' }
        ],
        averageWage: [
          { required: false, trigger: 'blur' },
          {
            validator: (rule, value, callback) => {
              if (value !== '' && isNaN(value)) {
                callback(new Error('月平均工资需为数字'));
              } else {
                callback();
              }
            }
          }
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
    //性别用途字典
    this.getDicts('sys_user_sex').then(response => {
      this.sexOptions = response.data
    })
    /** 查询部门下拉树结构 */
    getDeptNameInfo(this.$store.state.item.companyId, this.$store.state.item.itemNo).then(response => {
      this.deptIdOptions = response.data
      this.feeDeptIdOptions = response.data
    })

    this.getDetail(this.userInfoId)
  },
  methods: {
    /** 表单重置 */
    reset() {
      this.detail = {
        //附件列表
        attachmentList: [],
        education: null,
        id: null,
        name: null,
        deptId: null,
        jobNumber: null,
        sex: null,
        userType: null,
        feeDeptId: null,
        duties: null,
        employMethod: null,
        speciality: null,
        averageWage: null,
        title: null,
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

    /** 获取企业人员信息详情 */
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
            updateUserInfo(this.detail).then(response => {
              if (response.code === 200) {
                this.msgSuccess(response.msg)
                this.handleClose()
                this.handleQuery()
              }
            })
          } else {
            this.detail.companyId = this.$store.state.item.companyId
            this.detail.itemNo = this.$store.state.item.itemNo
            addUserInfo(this.detail).then(response => {
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
