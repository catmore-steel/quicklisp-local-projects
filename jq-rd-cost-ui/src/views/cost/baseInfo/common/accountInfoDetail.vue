<template>
  <jq-card :show="cardShow" :cardKey="cardKey" :title="title" :tags="tags" @handleClose="handleClose">
    <template slot="new">
      <div style="text-align: left;">
          <div>1、标记 * 为必须输入表单。</div>
      </div>
    </template>
    <template slot="operate" v-if="cardKey">
      <!-- <jq-check-user :userIds="[accountInfoForm.dealUserId]">-->
        <el-button icon="el-icon-edit" type="success" @click="handleUpdate"
                   v-hasPermi="['cost:accountInfo:edit']">
          修改
        </el-button>
        <el-button type="danger" icon="el-icon-delete" @click="handleRevoke()"
                 v-hasPermi="['cost:accountInfo:remove']">
          删除
        </el-button>
      <!--</jq-check-user>-->
    </template>
    <template>
      <el-tabs v-model="activeName">
        <el-tab-pane label="原始序时账信息" name="1">
            <accountInfo-update :accountInfoId="accountInfoId" :disabled.sync="disabled" @handleClose="handleClose"/>
        </el-tab-pane>
        <template v-if="value">
        </template>
      </el-tabs>
    </template>
  </jq-card>
</template>

<script>
  import accountInfoUpdate from './accountInfoUpdate'
  import {isNullOrEmpty} from '@/utils/jq'
  import {delAccountInfo, getAccountInfo} from '@/api/cost/accountInfo'

  export default {
    name: 'accountInfoDetail',
    components: {
      accountInfoUpdate,
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
        accountInfoForm: {},
        activeName: '1',
        cardShow: false,
        cardKey: null,
        tags: [],
        accountInfoId: null,
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

      // 获取原始序时账信息详情
      getDetail() {
        let accountInfoId = this.value
        if (isNullOrEmpty(accountInfoId)) {
          //新增
          this.title = '新增原始序时账信息'
          this.accountInfoId = null
          this.tags = null
        } else {
          getAccountInfo(accountInfoId).then(res => {
            let data = res.data
            this.accountInfoForm = res.data
            this.title = '原始序时账信息详情'
            this.accountInfoId = data.id
            this.tags = [{'日期': data.accDate}, {'凭证号': data.credentialsNo}, {'摘要': data.summary}, {'会计科目': data.accountSubject}]
          });
        }
      },

      /** 删除 */
      handleRevoke() {
        const ids = this.accountInfoId;
        this.$confirm('是否确认删除原始序时账信息编号为"' + ids + '"的数据项?', "警告", {
          confirmButtonText: "确定",
          cancelButtonText: "取消",
          type: "warning"
        }).then(function () {
          return delAccountInfo(ids);
        }).then(() => {
          this.msgSuccess("删除成功");
          this.handleClose()
          this.handleQuery()
        }).catch(() => {
        })
      },
    }
  }
</script>
