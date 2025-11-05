'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { Button } from '@/components/ui';
import { widgetService } from '@/services/widgetService';
import { Widget } from '@/types/widget';

export default function WidgetDetailPage() {
  const params = useParams();
  const router = useRouter();
  const widgetId = params.id as string;
  
  const [widget, setWidget] = useState<Widget | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    const fetchWidget = async () => {
      try {
        setLoading(true);
        setError(null);
        const data = await widgetService.getWidgetById(widgetId);
        setWidget(data);
      } catch (err) {
        console.error('Error fetching widget:', err);
        setError(err instanceof Error ? err.message : 'Failed to fetch widget');
      } finally {
        setLoading(false);
      }
    };

    if (widgetId) {
      fetchWidget();
    }
  }, [widgetId]);

  const handleEdit = () => {
    router.push(`/widgets/${widgetId}/edit`);
  };

  const handleDelete = async () => {
    if (!confirm('Are you sure you want to delete this widget?')) return;
    
    try {
      await widgetService.deleteWidget(widgetId);
      router.push('/widgets');
    } catch (err) {
      alert('Failed to delete widget');
      console.error(err);
    }
  };

  const handleBack = () => {
    router.push('/widgets');
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
      month: 'long',
      day: 'numeric',
    });
  };

  if (loading) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-gray-900"></div>
          <p className="mt-4 text-gray-600">Loading widget details...</p>
        </div>
      </div>
    );
  }

  if (error || !widget) {
    return (
      <div className="min-h-screen flex items-center justify-center">
        <div className="text-center">
          <div className="text-red-600 text-xl mb-4">Error</div>
          <p className="text-gray-600 mb-4">{error || 'Widget not found'}</p>
          <Button onClick={handleBack}>Back to Widgets</Button>
        </div>
      </div>
    );
  }

  return (
    <div className="container mx-auto px-4 py-8">
      <div className="mb-8">
        <div className="flex items-center justify-between">
          <div>
            <h1 className="text-3xl font-bold text-gray-900 mb-2">{widget.name}</h1>
            <p className="text-gray-600">Widget details and information</p>
          </div>
          <div className="flex gap-3">
            <Button variant="secondary" onClick={handleBack}>
              Back to List
            </Button>
            <Button onClick={handleEdit}>
              Edit Widget
            </Button>
            <Button variant="danger" onClick={handleDelete}>
              Delete
            </Button>
          </div>
        </div>
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        {/* Main Information Card */}
        <div className="lg:col-span-2 bg-white shadow-md rounded-lg overflow-hidden">
          <div className="bg-gray-50 px-6 py-4 border-b border-gray-200">
            <h2 className="text-xl font-semibold text-gray-900">Widget Information</h2>
          </div>
          
          <div className="p-6">
            <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
              {/* ID */}
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">
                  Widget ID
                </label>
                <p className="text-lg font-semibold text-gray-900">{widget.id}</p>
              </div>

              {/* Status */}
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">
                  Status
                </label>
                <span
                  className={`inline-flex items-center px-3 py-1 rounded-full text-sm font-medium ${
                    widget.isActive
                      ? 'bg-green-100 text-green-800'
                      : 'bg-red-100 text-red-800'
                  }`}
                >
                  {widget.isActive ? 'Active' : 'Inactive'}
                </span>
              </div>

              {/* Category */}
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">
                  Category
                </label>
                <p className="text-lg text-gray-900">{widget.category}</p>
              </div>

              {/* Price */}
              <div>
                <label className="block text-sm font-medium text-gray-500 mb-1">
                  Price
                </label>
                <p className="text-lg font-semibold text-gray-900">{formatCurrency(widget.price)}</p>
              </div>

              {/* Description */}
              {widget.description && (
                <div className="md:col-span-2">
                  <label className="block text-sm font-medium text-gray-500 mb-1">
                    Description
                  </label>
                  <p className="text-gray-900">{widget.description}</p>
                </div>
              )}
            </div>
          </div>
        </div>

        {/* Quick Stats Card */}
        <div className="bg-white shadow-md rounded-lg overflow-hidden">
          <div className="bg-gray-50 px-6 py-4 border-b border-gray-200">
            <h2 className="text-xl font-semibold text-gray-900">Quick Stats</h2>
          </div>
          
          <div className="p-6 space-y-4">
            <div>
              <p className="text-sm text-gray-500 mb-1">Price</p>
              <p className="text-2xl font-bold text-gray-900">
                {formatCurrency(widget.price)}
              </p>
            </div>
            
            <div className="pt-4 border-t border-gray-200">
              <p className="text-sm text-gray-500 mb-1">Category</p>
              <p className="text-xl font-semibold text-gray-900">
                {widget.category}
              </p>
            </div>

            <div className="pt-4 border-t border-gray-200">
              <p className="text-sm text-gray-500 mb-1">Status</p>
              <span
                className={`inline-flex items-center px-3 py-1 rounded-full text-sm font-medium ${
                  widget.isActive
                    ? 'bg-green-100 text-green-800'
                    : 'bg-red-100 text-red-800'
                }`}
              >
                {widget.isActive ? 'Active' : 'Inactive'}
              </span>
            </div>
          </div>
        </div>
      </div>

      {/* Date Information */}
      <div className="mt-6 bg-white shadow-md rounded-lg overflow-hidden">
        <div className="bg-gray-50 px-6 py-4 border-b border-gray-200">
          <h2 className="text-xl font-semibold text-gray-900">Timeline</h2>
        </div>
        
        <div className="p-6">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            {/* Created Date */}
            <div>
              <label className="block text-sm font-medium text-gray-500 mb-2">
                Created Date
              </label>
              <p className="text-lg text-gray-900">{formatDate(widget.createdAt)}</p>
            </div>

            {/* Updated Date */}
            <div>
              <label className="block text-sm font-medium text-gray-500 mb-2">
                Last Updated
              </label>
              <p className="text-lg text-gray-900">{formatDate(widget.updatedAt)}</p>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
