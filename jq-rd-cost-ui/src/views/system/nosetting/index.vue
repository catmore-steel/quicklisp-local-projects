<template>
  <div class="app-container">
    <el-form :model="queryParams" ref="queryForm" :inline="true"  label-width="68px">
      <el-form-item label="名称" prop="settingName">
        <el-input
          v-model="queryParams.settingName"
          placeholder="请输入名称"
          clearable

          @keyup.enter.native="handleQuery"
        />
      </el-form-item>
      <el-form-item>
        <el-button type="cyan" icon="el-icon-search"  @click="handleQuery">搜索</el-button>
        <el-button icon="el-icon-refresh"  @click="resetQuery">重置</el-button>
      </el-form-item>
    </el-form>

    <el-row :gutter="10" class="table-opt mb8">
      <el-col :span="1.5">
        <el-button
          type="primary"
          icon="el-icon-plus"

          @click="handleAdd"
          v-hasPermi="['system:nosetting:add']"
        >新增
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button
          type="success"
          icon="el-icon-edit"

          :disabled="single"
          @click="handleUpdate"
          v-hasPermi="['system:nosetting:edit']"
        >修改
        </el-button>
      </el-col>
      <el-col :span="1.5">
        <el-button
          type="danger"
          icon="el-icon-delete"

          :disabled="multiple"
          @click="handleDelete"
          v-hasPermi="['system:nosetting:remove']"
        >删除
        </el-button>
      </el-col>
      <!--<el-col :span="1.5">
        <el-button
          type="warning"
          icon="el-icon-download"

          @click="handleExport"
          v-hasPermi="['system:nosetting:export']"
        >导出
        </el-button>
      </el-col>-->
      <right-toolbar :showSearch.sync="showSearch" @queryTable="getList"></right-toolbar>
    </el-row>

    <el-table v-loading="loading" :data="nosettingList" @selection-change="handleSelectionChange">
      <el-table-column type="selection" width="55" align="center"/>
      <el-table-column label="主键" align="center" prop="id"/>
      <el-table-column label="编号" align="center" prop="settingNo"/>
      <el-table-column label="名称" align="center" prop="settingName"/>
      <el-table-column label="规则" align="center" prop="ruleName" width="300"/>
      <el-table-column label="最新日期号" align="center" prop="runDate"/>
      <el-table-column label="最新流水号" align="center" prop="detailNo"/>
      <el-table-column label="当前单号" align="center" prop="currentNo"/>
      <el-table-column label="状态" align="center" prop="delFlag" :formatter="delFlagFormat"/>
      <el-table-column label="操作" align="center" class-name="small-padding fixed">
        <template slot-scope="scope">
          <el-button

            type="text"
            icon="el-icon-edit"
            @click="handleUpdate(scope.row)"
            v-hasPermi="['system:nosetting:edit']"
          >修改
          </el-button>
          <el-button

            type="text"
            icon="el-icon-delete"
            @click="handleDelete(scope.row)"
            v-hasPermi="['system:nosetting:remove']"
          >删除
          </el-button>
        </template>
      </el-table-column>
    </el-table>

    <pagination
      v-show="total>0"
      :total="total"
      :page.sync="queryParams.pageNum"
      :limit.sync="queryParams.pageSize"
      @pagination="getList"
    />

    <!-- 添加或修改单号自动获取设置对话框 -->
    <jq-dialog :title="title" :visible.sync="open" width="500px" :close-on-click-modal="false" append-to-body>
      <el-form ref="form" :model="form" :rules="rules" label-width="80px">
        <el-form-item label="规则编码" prop="settingNo">
          <el-input v-model="form.settingNo" placeholder="请输入规则编码"   :readonly="this.form.id != null"/>
        </el-form-item>
        <el-form-item label="名称" prop="settingName">
          <el-input v-model="form.settingName" placeholder="请输入名称"/>
        </el-form-item>
        <el-form-item label="规则" prop="ruleArr">
          <el-select v-model="form.ruleArr" multiple placeholder="请选择规则" class="el-input">
            <el-option
              v-for="dict in ruleOptions"
              :key="dict.dictValue"
              :label="dict.dictLabel"
              :value="dict.dictValue"
            ></el-option>
          </el-select>
        </el-form-item>
        <el-form-item label="状态">
          <el-radio-group v-model="form.delFlag">
            <el-radio
              v-for="dict in delFlagOptions"
              :key="dict.dictValue"
              :label="dict.dictValue"
            >{{ dict.dictLabel }}
            </el-radio>
          </el-radio-group>
        </el-form-item>
      </el-form>
      <div slot="footer" >
        <el-button type="primary" @click="submitForm">确 定</el-button>
        <el-button @click="cancel">取 消</el-button>
      </div>
    </jq-dialog>
  </div>
</template>

<script>
import {
  listNosetting,
  getNosetting,
  delNosetting,
  addNosetting,
  updateNosetting,
  exportNosetting
} from "@/api/system/nosetting";

export default {
  name: "Nosetting",
  data() {
    return {
      // 遮罩层
      loading: true,
      // 选中数组
      ids: [],
      // 非单个禁用
      single: true,
      // 非多个禁用
      multiple: true,
      // 显示搜索条件
      showSearch: true,
      // 总条数
      total: 0,
      // 单号自动获取设置表格数据
      nosettingList: [],
      // 弹出层标题
      title: "",
      // 是否显示弹出层
      open: false,
      // 规则字典
      ruleOptions: [],
      // 删除标志字典
      delFlagOptions: [],
      // 查询参数
      queryParams: {
        pageNum: 1,
        pageSize: 15,
        settingName: null,
      },
      // 表单参数
      form: {},
      // 表单校验
      rules: {
        settingNo: [
          {required: true, message: "规则编码不能为空，且不能重复", trigger: "blur"}
        ],
        settingName: [
          {required: true, message: "规则名称不能为空", trigger: "blur"}
        ],
        ruleArr: [
          {required: true, message: "规则不能为空", trigger: "blur"}
        ]
      }
    };
  },
  created() {
    this.getList();
    this.getDicts("no_setting").then(response => {
      this.ruleOptions = response.data;
    });
    this.getDicts("sys_normal_disable").then(response => {
      this.delFlagOptions = response.data;
    });
  },
  methods: {
    /** 查询单号自动获取设置列表 */
    getList() {
      this.loading = true;
      listNosetting(this.queryParams).then(response => {
        this.nosettingList = response.rows;
        this.total = response.total;
        this.loading = false;
      });
    },
    // 规则字典翻译
    ruleFormat(row, column) {
      return this.selectDictLabel(this.ruleOptions, row.rule);
    },
    // 删除标志字典翻译
    delFlagFormat(row, column) {
      return this.selectDictLabel(this.delFlagOptions, row.delFlag);
    },
    // 取消按钮
    cancel() {
      this.open = false;
      this.reset();
    },
    // 表单重置
    reset() {
      this.form = {
        id: null,
        settingNo: null,
        settingName: null,
        ruleArr: null,
        delFlag: "0",
        createBy: null,
        createTime: null,
        updateBy: null,
        updateTime: null,
        remark: null
      };
      this.resetForm("form");
    },
    /** 搜索按钮操作 */
    handleQuery() {
      this.queryParams.pageNum = 1;
      this.getList();
    },
    /** 重置按钮操作 */
    resetQuery() {
      this.resetForm("queryForm");
      this.handleQuery();
    },
    // 多选框选中数据
    handleSelectionChange(selection) {
      this.ids = selection.map(item => item.id)
      this.single = selection.length !== 1
      this.multiple = !selection.length
    },
    /** 新增按钮操作 */
    handleAdd() {
      this.reset();
      this.open = true;
      this.title = "添加单号自动获取设置";
    },
    /** 修改按钮操作 */
    handleUpdate(row) {
      this.reset();
      const id = row.id || this.ids
      getNosetting(id).then(response => {
        this.form = response.data;
        this.open = true;
        this.title = "修改单号自动获取设置";
      });
    },
    /** 提交按钮 */
    submitForm() {
      this.$refs["form"].validate(valid => {
        if (valid) {
          if (this.form.id != null) {
            updateNosetting(this.form).then(response => {
              this.msgSuccess("修改成功");
              this.open = false;
              this.getList();
            });
          } else {
            addNosetting(this.form).then(response => {
              this.msgSuccess("新增成功");
              this.open = false;
              this.getList();
            });
          }
        }
      });
    },
    /** 删除按钮操作 */
    handleDelete(row) {
      const ids = row.id || this.ids;
      this.$confirm('是否确认删除单号自动获取设置编号为"' + ids + '"的数据项?', "警告", {
        confirmButtonText: "确定",
        cancelButtonText: "取消",
        type: "warning"
      }).then(function () {
        return delNosetting(ids);
      }).then(() => {
        this.getList();
        this.msgSuccess("删除成功");
      })
    },
    /** 导出按钮操作 */
    handleExport() {
      const queryParams = this.queryParams;
      this.$confirm('是否确认导出所有单号自动获取设置数据项?', "警告", {
        confirmButtonText: "确定",
        cancelButtonText: "取消",
        type: "warning"
      }).then(function () {
        return exportNosetting(queryParams);
      }).then(response => {
        this.download(response.msg);
      })
    }
  }
};
</script>

<style>

</style>
