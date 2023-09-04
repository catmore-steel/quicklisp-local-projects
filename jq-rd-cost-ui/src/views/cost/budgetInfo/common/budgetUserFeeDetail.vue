<template>
  <jq-card :show="cardShow" :cardKey="cardKey" :title="title" :tags="tags" @handleClose="handleClose">
    <template slot="new">
      <div style="text-align: left;">
          <div>1、标记 * 为必须输入表单。</div>
      </div>
    </template>
    <template slot="operate" v-if="cardKey">
      <!-- <jq-check-user :userIds="[budgetUserFeeForm.dealUserId]">-->
        <el-button icon="el-icon-edit" type="success" @click="handleUpdate"
                   v-hasPermi="['cost:budgetUserFee:edit']">
          修改
        </el-button>
      <!--</jq-check-user>-->
    </template>
    <template>
      <el-tabs v-model="activeName">
        <el-tab-pane label="投入研发信息" name="1">
          <budgetUserFee-update :userInfoId="value" :disabled.sync="disabled" @handleClose="handleClose"/>
        </el-tab-pane>

      </el-tabs>
    </template>
  </jq-card>
</template>

<script>
  import budgetUserFeeUpdate from './budgetUserFeeUpdate'
  import {isNullOrEmpty} from '@/utils/jq'
  import { delBudgetUserFee, getBaseUserInfo, getBudgetUserFee } from '@/api/cost/budgetUserFee'
  import { getUserInfo } from '@/api/cost/userInfo'

  export default {
    name: 'budgetUserFeeDetail',
    components: {
      budgetUserFeeUpdate,
    },
    provide() {
      return {
        handleComplete: this.handleComplete
      }
    },
    inject: ['handleQuery'],
    data() {
      return {
        title: '',
        budgetUserFeeForm: {},
        userInfoForm:{},
        activeName: '1',
        cardShow: false,
        cardKey: null,
        tags: [],
        userInfoId: null,
        budgetUserFeeId: null,
        edit: false,  //开启编辑
        disabled: false,
      }
    },
    props: {
      show: {
        type: Boolean,
        default: false
      },
      value: {
        type: Number
      }
    },
    watch: {
      show(data) {
        this.cardShow = data
      },
      value(data) {
        this.activeName = '1'
        this.cardKey = data
        if (data) {
            this.disabled = true
        } else {
            this.disabled = false
        }
        this.getDetail()
      }
    },
    created() {
      this.getDetail()
    },
    mounted() {
    },
    methods: {
      /** 修改操作 */
      handleUpdate() {
        this.disabled = false
      },

      /** 关闭 */
      handleClose() {
        this.cardShow = false
        this.cardKey = null
        this.$emit('handleClose')
      },

      // 获取人员费用拟定;详情
      getDetail() {
        let userInfoId = this.value
        if (isNullOrEmpty(userInfoId)) {
          //新增
          this.title = '新增人员'
          this.userInfoForm = {}
          this.userInfoId = null
          this.tags = null
        } else {
          getUserInfo(userInfoId).then(res => {
            this.title = '人员详情'
            let data = res.data
            this.userInfoForm = res.data
            this.userInfoId = data.id
            this.tags = [{'员工姓名': data.name}, {'人员类型': data.userTypeName}, {'总工时': data.sumHour}, {'投入研发工时': data.sumDevHours}]
          });
        }
      },
      /*// 获取企业人员信息详情
      getDetail() {
        let userInfoId = this.value
        if (isNullOrEmpty(userInfoId)) {
          //新增
          this.title = '新增人员'
          this.userInfoId = null
          this.tags = null
        } else {
          getUserInfo(userInfoId).then(res => {
            let data = res.data
            this.userInfoForm = res.data
            this.title = '人员详情'
            this.userInfoId = data.id
            this.tags = [{'员工姓名': data.name}, {'人员类型': data.userType}, {'总工时': data.sumHours}, {'投入研发工时': data.devHours}]
          });
        }
      },*/

    }
  }
</script>
