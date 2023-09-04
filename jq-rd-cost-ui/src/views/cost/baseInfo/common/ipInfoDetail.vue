<template>
  <jq-card :show="cardShow" :cardKey="cardKey" :title="title" :tags="tags" @handleClose="handleClose">
    <template slot="new">
      <div style="text-align: left;">
        <div>1、标记 * 为必须输入表单。</div>
      </div>
    </template>
    <template slot="operate" v-if="cardKey">
      <!-- <jq-check-user :userIds="[ipInfoForm.dealUserId]">-->
      <el-button icon="el-icon-edit" type="success" @click="handleUpdate"
                 v-hasPermi="['cost:ipInfo:edit']">
        修改
      </el-button>
      <el-button type="danger" icon="el-icon-delete" @click="handleRevoke()"
                 v-hasPermi="['cost:ipInfo:remove']">
        删除
      </el-button>
      <!--</jq-check-user>-->
    </template>
    <template>
      <el-tabs v-model="activeName">
        <el-tab-pane label="无形资产信息" name="1">
          <ipInfo-update :ipInfoId="ipInfoId" :disabled.sync="disabled" @handleClose="handleClose"/>
        </el-tab-pane>
      </el-tabs>
    </template>
  </jq-card>
</template>

<script>
  import ipInfoUpdate from './ipInfoUpdate'
  import { isNullOrEmpty } from '@/utils/jq'
  import { delIpInfo, getIpInfo } from '@/api/cost/ipInfo'

  export default {
    name: 'ipInfoDetail',
    components: {
      ipInfoUpdate
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
        ipInfoForm: {},
        activeName: '1',
        cardShow: false,
        cardKey: null,
        tags: [],
        ipInfoId: null,
        edit: false,  //开启编辑
        disabled: false
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

      /** 审核操作 */
      handleExamine() {
        this.disabled = false
      },
      /** 关闭 */
      handleClose() {
        this.cardShow = false
        this.cardKey = null
        this.$emit('handleClose')
      },

      // 获取无形资产信息详情
      getDetail() {
        let ipInfoId = this.value
        if (isNullOrEmpty(ipInfoId)) {
          //新增
          this.title = '新增资产'
          this.ipInfoId = null
          this.tags = null
        } else {
          getIpInfo(ipInfoId).then(res => {
            let data = res.data
            this.ipInfoForm = res.data
            this.title = '资产详情'
            this.ipInfoId = data.id
            this.tags = [{ '资产编号': data.ipNo }, { '资产名称': data.ipName }, { '登记编号': data.registeNo }, { '取得日期': data.getDate }]
          })
        }
      },

      /** 撤销 */
      handleRevoke() {
        const ids = this.ipInfoId
        this.$confirm('是否确认撤销无形资产信息编号为"' + ids + '"的数据项?', '警告', {
          confirmButtonText: '确定',
          cancelButtonText: '取消',
          type: 'warning'
        }).then(function() {
          return delIpInfo(ids)
        }).then(() => {
          this.msgSuccess('删除成功')
          this.handleClose()
          this.handleQuery()
        }).catch(() => {
        })
      }
    }
  }
</script>
