<template>
  <jq-card :show="cardShow" :cardKey="cardKey" :title="title" :tags="tags" @handleClose="handleClose">
    <template slot="new">
      <div style="text-align: left;">
          <div>1、标记 * 为必须输入表单。</div>
      </div>
    </template>
    <template slot="operate" v-if="cardKey">
      <!-- <jq-check-user :userIds="[confmenuForm.dealUserId]">-->
        <el-button icon="el-icon-edit" type="success" @click="handleUpdate"
                   v-hasPermi="['system:confmenu:edit']">
          修改
        </el-button>
        <el-button type="danger" icon="el-icon-delete" @click="handleRevoke()"
                 v-hasPermi="['system:confmenu:remove']">
          撤销
        </el-button>
      <!--</jq-check-user>-->
    </template>
    <template>
      <el-tabs v-model="activeName">
        <el-tab-pane label="整本材料目录配置" name="1">
            <confmenu-update :confmenuId="confmenuId" :disabled.sync="disabled" @handleClose="handleClose"/>
        </el-tab-pane>
        <!--<template v-if="value">
            <el-tab-pane label="测试一" name="2" lazy></el-tab-pane>
        </template> -->
      </el-tabs>
    </template>
  </jq-card>
</template>

<script>
  import confmenuUpdate from './confmenuUpdate'
  import {isNullOrEmpty} from '@/utils/jq'
  import {delConfmenu, getConfmenu} from '@/api/system/confmenu'

  export default {
    name: 'confmenuDetail',
    components: {
      confmenuUpdate,
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
        confmenuForm: {},
        activeName: '1',
        cardShow: false,
        cardKey: null,
        tags: [],
        confmenuId: null,
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

      // 获取整本材料目录配置详情
      getDetail() {
        let confmenuId = this.value
        if (isNullOrEmpty(confmenuId)) {
          //新增
          this.title = '新增整本材料目录配置'
          this.confmenuId = null
          this.tags = null
        } else {
          getConfmenu(confmenuId).then(res => {
            let data = res.data
            this.confmenuForm = res.data
            this.title = '整本材料目录配置详情'
            this.confmenuId = data.id
            this.tags = [{'创建人': data.createBy}, {'创建时间': data.createTime}, {'更新人': data.updateBy}, {'更新时间': data.updateTime}]
          });
        }
      },

      /** 撤销 */
      handleRevoke() {
        const ids = this.confmenuId;
        this.$confirm('是否确认撤销整本材料目录配置编号为"' + ids + '"的数据项?', "警告", {
          confirmButtonText: "确定",
          cancelButtonText: "取消",
          type: "warning"
        }).then(function () {
          return delConfmenu(ids);
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
