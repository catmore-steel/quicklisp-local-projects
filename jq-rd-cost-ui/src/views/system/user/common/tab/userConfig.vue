<!-- 费用减缓 -->
<template>
  <div>
    <el-button type="primary" @click="addUserConfig">添加配置</el-button>
    <el-table :data="configData">
      <el-table-column label="操作" align="center">
        <template slot-scope="scope">
          <el-button type="text" icon="el-icon-delete" @click="deleteUserConfig(scope.row)">
            删除
          </el-button>
        </template>
      </el-table-column>
      <el-table-column prop="typeName" label="配置类型"></el-table-column>
      <el-table-column prop="createBy" label="创建人"></el-table-column>
      <el-table-column prop="createTime" label="创建时间"></el-table-column>
    </el-table>

    <pagination
      v-show="total>0"
      :total="total"
      :page.sync="queryParams.pageNum"
      :limit.sync="queryParams.pageSize"
      @pagination="getConfigData"
    />

    <jq-dialog :title="title" :visible.sync="open" width="600px" append-to-body>
      <el-form ref="form" :model="configForm" :rules="rules" label-width="100px">
        <el-row>
          <el-col :span="24">
            <el-form-item label="配置类型" prop="type">
              <el-select v-model="configForm.type" placeholder="配置类型" style="width: 100%">
                <el-option
                  v-for="dict in resultOptions"
                  :key="dict.dictValue"
                  :label="dict.dictLabel"
                  :value="dict.dictValue"
                />
              </el-select>
            </el-form-item>
          </el-col>
        </el-row>
      </el-form>
      <div slot="footer" style="text-align: center">
        <el-button type="primary" @click="submitConfigForm">确 定</el-button>
        <el-button @click="cancel">取 消</el-button>
      </div>
    </jq-dialog>
  </div>

</template>

<script>
  import { addConfig, delConfig, listUserConfig, updateConfig } from '@/api/system/userConfig'

export default {
  name: 'userConfig',
  data() {
    return {
      // 弹出层标题
      title: '',
      // 显示搜索条件
      showSearch: true,
      // 总条数
      total: 0,
      // 是否显示弹出层
      open: false,
      configData: [],
      costRateYear: [],
      //配置类型
      resultOptions: [],
      configForm: {
        userId: null
      },
      queryParams: {
        userId: null,
        pageNum: 1,
        pageSize: 15,
        exportType: 1,
        name: null

      },
      rules: {
        type: [
          { required: true, message: '配置类型不能为空', trigger: 'blur' }
        ],
      }
    }
  },
  created() {
    this.getDicts('user_config_type').then(response => {
      this.resultOptions = response.data
    }),
    this.getConfigData()
  },
  props: {
    userId: {
      type: Number,
      default: 0
    }
  },
  watch: {
    userId: {
      handler() {
        this.getConfigData()
      },
      deep: true
    }
  },
  methods: {
    // 获取费用减缓的表格
    async getConfigData() {
      const userId = this.userId
      const data = await listUserConfig({ userId: userId })
      if (data && data.code === 200) {
        this.configData = data.rows
        this.total = data.total
        this.loading = false
      }
    },
    // 取消按钮
    cancel() {
      this.open = false
      this.reset()
    },
    reset() {
      this.configForm = {
        userId: null,
        type: null
      }
      this.resetForm('form')
    },
    /** 新增按钮操作 */
    addUserConfig() {
      this.reset()
      this.open = true
      this.title = '新增配置'
    },
    /** 删除按钮操作 */
    deleteUserConfig(row) {
      const ids = row.id || this.ids
      this.$confirm('是否确认删除?', '警告', {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }).then(function() {
        return delConfig(ids)
      }).then(() => {
        this.getConfigData()
        this.msgSuccess('删除成功')
      })
    },
    /** 提交按钮 */
    submitConfigForm() {
      this.configForm.userId = this.userId
      this.$refs['form'].validate(valid => {
        if (valid) {
          if (this.configForm.id != null) {
            updateConfig(this.configForm).then(response => {
              this.msgSuccess('修改成功')
              this.open = false
              this.getConfigData()
            })
          } else {
            addConfig(this.configForm).then(response => {
              this.msgSuccess('新增成功')
              this.open = false
              this.getConfigData()
            })
          }
        }
      })
    }
  }
}
</script>

<style scoped>

</style>
