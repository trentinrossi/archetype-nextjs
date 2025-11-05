'use client';

import React, { useEffect, useState } from 'react';
import { useRouter } from 'next/navigation';
import { widgetService } from '@/services/widgetService';
import { Widget } from '@/types/widget';
import { Table, Button, Select, Modal } from '@/components/ui';

export default function WidgetsPage() {
  const router = useRouter();
  const [widgets, setWidgets] = useState<Widget[]>([]);
  const [filteredWidgets, setFilteredWidgets] = useState<Widget[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [categoryFilter, setCategoryFilter] = useState<string>('all');
  const [statusFilter, setStatusFilter] = useState<string>('all');
  const [uniqueCategories, setUniqueCategories] = useState<string[]>([]);
  const [deleteModalOpen, setDeleteModalOpen] = useState(false);
  const [widgetToDelete, setWidgetToDelete] = useState<string | null>(null);
  const [deleting, setDeleting] = useState(false);

  const fetchWidgets = async () => {
    try {
      setLoading(true);
      setError(null);
      const data = await widgetService.getWidgets();
      setWidgets(data);
      setFilteredWidgets(data);

      const categories = Array.from(new Set(data.map(widget => widget.category))).sort();
      setUniqueCategories(categories);
    } catch (err) {
      console.error('Error fetching widgets:', err);
      setError(err instanceof Error ? err.message : 'An unexpected error occurred');
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    fetchWidgets();
  }, []);

  useEffect(() => {
    let filtered = [...widgets];

    if (categoryFilter !== 'all') {
      filtered = filtered.filter(widget => widget.category === categoryFilter);
    }

    if (statusFilter !== 'all') {
      filtered = filtered.filter(widget => 
        statusFilter === 'active' ? widget.isActive : !widget.isActive
      );
    }

    setFilteredWidgets(filtered);
  }, [categoryFilter, statusFilter, widgets]);

  const handleViewDetails = (id: string) => {
    router.push(`/widgets/${id}`);
  };

  const handleEdit = (id: string) => {
    router.push(`/widgets/${id}/edit`);
  };

  const handleDeleteClick = (id: string) => {
    setWidgetToDelete(id);
    setDeleteModalOpen(true);
  };

  const handleDeleteConfirm = async () => {
    if (!widgetToDelete) return;

    try {
      setDeleting(true);
      await widgetService.deleteWidget(widgetToDelete);
      setWidgets(prev => prev.filter(widget => widget.id !== widgetToDelete));
      setDeleteModalOpen(false);
      setWidgetToDelete(null);
    } catch (err) {
      console.error('Error deleting widget:', err);
      setError(err instanceof Error ? err.message : 'Failed to delete widget');
    } finally {
      setDeleting(false);
    }
  };

  const handleDeleteCancel = () => {
    setDeleteModalOpen(false);
    setWidgetToDelete(null);
  };

  const handleCreateNew = () => {
    router.push('/widgets/new');
  };

  const formatCurrency = (amount: number): string => {
    return new Intl.NumberFormat('en-US', {
      style: 'currency',
      currency: 'USD',
    }).format(amount);
  };

  const formatDate = (dateString: string): string => {
    if (!dateString) return 'N/A';
    return new Date(dateString).toLocaleDateString('en-US', {
      year: 'numeric',
      month: 'short',
      day: 'numeric',
    });
  };

  const columns = [
    { key: 'id', label: 'ID' },
    { key: 'name', label: 'Name' },
    { key: 'category', label: 'Category' },
    { key: 'price', label: 'Price' },
    { key: 'isActive', label: 'Status' },
    { key: 'createdAt', label: 'Created' },
    { key: 'actions', label: 'Actions' },
  ];

  const tableData = filteredWidgets.map(widget => ({
    id: <span className="font-medium text-gray-900">{widget.id}</span>,
    name: <span className="font-medium text-gray-900">{widget.name}</span>,
    category: <span className="text-gray-900">{widget.category}</span>,
    price: <span className="text-gray-900">{formatCurrency(widget.price)}</span>,
    isActive: (
      <span
        className={`inline-flex items-center px-2.5 py-0.5 rounded-full text-xs font-medium ${
          widget.isActive
            ? 'bg-green-100 text-green-800'
            : 'bg-red-100 text-red-800'
        }`}
      >
        {widget.isActive ? 'Active' : 'Inactive'}
      </span>
    ),
    createdAt: <span className="text-gray-900">{formatDate(widget.createdAt)}</span>,
    actions: (
      <div className="flex gap-2">
        <Button
          variant="secondary"
          size="sm"
          onClick={() => handleViewDetails(widget.id)}
        >
          View
        </Button>
        <Button
          variant="secondary"
          size="sm"
          onClick={() => handleEdit(widget.id)}
        >
          Edit
        </Button>
        <Button
          variant="danger"
          size="sm"
          onClick={() => handleDeleteClick(widget.id)}
        >
          Delete
        </Button>
      </div>
    ),
  }));

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-gray-900"></div>
          <p className="mt-4 text-gray-600">Loading widgets...</p>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="text-red-600 text-xl mb-4">Error</div>
          <p className="text-gray-600 mb-4">{error}</p>
          <Button onClick={fetchWidgets}>Retry</Button>
        </div>
      </div>
    );
  }

  return (
    <div className="container mx-auto px-4 py-8">
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Widgets</h1>
        <p className="text-gray-600">Manage and view all widgets</p>
      </div>

      <div className="mb-6 flex flex-wrap gap-4 items-end">
        <div className="flex-1 min-w-[200px]">
          <label htmlFor="category-filter" className="block text-sm font-medium text-gray-700 mb-1">
            Filter by Category
          </label>
          <Select
            id="category-filter"
            value={categoryFilter}
            onChange={(e) => setCategoryFilter(e.target.value)}
            options={[
              { value: 'all', label: 'All Categories' },
              ...uniqueCategories.map(category => ({
                value: category,
                label: category,
              })),
            ]}
          />
        </div>

        <div className="flex-1 min-w-[200px]">
          <label htmlFor="status-filter" className="block text-sm font-medium text-gray-700 mb-1">
            Filter by Status
          </label>
          <Select
            id="status-filter"
            value={statusFilter}
            onChange={(e) => setStatusFilter(e.target.value)}
            options={[
              { value: 'all', label: 'All Statuses' },
              { value: 'active', label: 'Active' },
              { value: 'inactive', label: 'Inactive' },
            ]}
          />
        </div>

        <div className="flex-shrink-0">
          <Button onClick={handleCreateNew}>Create New Widget</Button>
        </div>
      </div>

      {filteredWidgets.length === 0 ? (
        <div className="text-center py-12 bg-gray-50 rounded-lg">
          <svg
            className="mx-auto h-12 w-12 text-gray-400"
            fill="none"
            viewBox="0 0 24 24"
            stroke="currentColor"
            aria-hidden="true"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth={2}
              d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"
            />
          </svg>
          <h3 className="mt-2 text-sm font-medium text-gray-900">No widgets found</h3>
          <p className="mt-1 text-sm text-gray-500">
            {widgets.length === 0
              ? 'Get started by creating a new widget.'
              : 'Try adjusting your filters to see more results.'}
          </p>
          {widgets.length === 0 && (
            <div className="mt-6">
              <Button onClick={handleCreateNew}>Create New Widget</Button>
            </div>
          )}
        </div>
      ) : (
        <div className="bg-white shadow-md rounded-lg overflow-hidden">
          <Table columns={columns} data={tableData} />
          <div className="px-6 py-4 bg-gray-50 border-t border-gray-200">
            <p className="text-sm text-gray-700">
              Showing <span className="font-medium">{filteredWidgets.length}</span> of{' '}
              <span className="font-medium">{widgets.length}</span> widgets
            </p>
          </div>
        </div>
      )}

      <Modal
        isOpen={deleteModalOpen}
        onClose={handleDeleteCancel}
        title="Delete Widget"
      >
        <div className="mt-2">
          <p className="text-sm text-gray-500">
            Are you sure you want to delete this widget? This action cannot be undone.
          </p>
        </div>
        <div className="mt-6 flex justify-end gap-3">
          <Button
            variant="secondary"
            onClick={handleDeleteCancel}
            disabled={deleting}
          >
            Cancel
          </Button>
          <Button
            variant="danger"
            onClick={handleDeleteConfirm}
            disabled={deleting}
          >
            {deleting ? 'Deleting...' : 'Delete'}
          </Button>
        </div>
      </Modal>
    </div>
  );
}
