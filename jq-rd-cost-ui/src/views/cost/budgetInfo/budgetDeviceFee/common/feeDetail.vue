<template>
  <jq-card :show="cardShow" :cardKey="cardKey" :title="title" :tags="tags" @handleClose="handleClose">
    <template slot="new">
      <div style="text-align: left;">
          <div>1、标记 * 为必须输入表单。</div>
      </div>
    </template>
    <template slot="operate" v-if="cardKey">
      <!-- <jq-check-user :userIds="[feeForm.dealUserId]">-->
        <el-button icon="el-icon-edit" type="success" @click="handleUpdate"
                   v-hasPermi="['system:fee:edit']">
          修改
        </el-button>
        <!--<el-button type="danger" icon="el-icon-delete" @click="handleRevoke()"
                 v-hasPermi="['system:fee:remove']">
          撤销
        </el-button>
      </jq-check-user>-->
    </template>
    <template>
      <el-tabs v-model="activeName">
        <el-tab-pane label="投入研发信息" name="1">
            <fee-update :feeId="feeId" :disabled.sync="disabled" @handleClose="handleClose"/>
        </el-tab-pane>
      </el-tabs>
    </template>
  </jq-card>
</template>

<script>
  import feeUpdate from './feeUpdate'
  import {isNullOrEmpty} from '@/utils/jq'
  import {delByDeviceInfoId, getBaseDeviceInfo} from '@/api/cost/budgetDeviceFee.js'

  export default {
    name: 'feeDetail',
    components: {
      feeUpdate,
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
        feeForm: {},
        activeName: '1',
        cardShow: false,
        cardKey: null,
        tags: [],
        feeId: null,
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

      // 获取研发投入设备折旧费用拟定;详情
      getDetail() {
        let feeId = this.value
        if (isNullOrEmpty(feeId)) {
          //新增
          this.title = '新增研发投入设备折旧费用拟定;'
          this.feeId = null
          this.tags = null
        } else {
          getBaseDeviceInfo(feeId).then(res => {
            let data = res.data
            this.feeForm = res.data
            this.title = '研发投入设备折旧费用拟定;详情'
            this.feeId = data.id
            this.tags = [{'创建人': data.createBy}, {'创建时间': data.createTime}, {'更新人': data.updateBy}, {'更新时间': data.updateTime}]
          });
        }
      },

      /** 撤销 */
      handleRevoke() {
        const ids = this.feeId;
        this.$confirm('是否确认撤销研发投入设备折旧费用拟定;编号为"' + ids + '"的数据项?', "警告", {
          confirmButtonText: "确定",
          cancelButtonText: "取消",
          type: "warning"
        }).then(function () {
          return delByDeviceInfoId(ids);
        }).then(() => {
          this.msgSuccess("删除成功");
          this.handleClose()
          this.$emit('handleQuery')
        }).catch(() => {
        })
      },
    }
  }
</script>
