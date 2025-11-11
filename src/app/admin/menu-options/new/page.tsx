'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { adminMenuOptionService } from '@/services/adminMenuOptionService';
import { CreateAdminMenuOptionRequest } from '@/types/admin-menu-option';
import { Input, Button } from '@/components/ui';

export default function CreateAdminMenuOptionPage() {
  const router = useRouter();
  const [formData, setFormData] = useState<CreateAdminMenuOptionRequest>({
    optionNumber: 0,
    optionName: '',
    programName: '',
    isActive: true,
    adminUserId: '',
  });
  const [loading, setLoading] = useState(false);
  const [errors, setErrors] = useState<Record<string, string>>({});

  const validateForm = (): boolean => {
    const newErrors: Record<string, string> = {};

    if (formData.optionNumber < 0 || formData.optionNumber > 99) {
      newErrors.optionNumber = 'Option number must be between 0 and 99';
    }

    if (!formData.optionName.trim()) {
      newErrors.optionName = 'Option name is required';
    } else if (formData.optionName.length > 35) {
      newErrors.optionName = 'Option name must be 35 characters or less';
    }

    if (!formData.programName.trim()) {
      newErrors.programName = 'Program name is required';
    } else if (formData.programName.length > 8) {
      newErrors.programName = 'Program name must be 8 characters or less';
    }

    if (!formData.adminUserId.trim()) {
      newErrors.adminUserId = 'Admin user ID is required';
    } else if (formData.adminUserId.length > 8) {
      newErrors.adminUserId = 'Admin user ID must be 8 characters or less';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    
    if (!validateForm()) {
      return;
    }

    try {
      setLoading(true);
      await adminMenuOptionService.createAdminMenuOption(formData);
      router.push('/admin/menu-options');
    } catch (err) {
      alert('Failed to create menu option. Option number may already exist.');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="p-6 max-w-2xl">
      <h1 className="text-2xl font-bold mb-6">Create New Menu Option</h1>
      
      <form onSubmit={handleSubmit} className="space-y-4">
        <div>
          <Input
            label="Option Number"
            type="number"
            value={formData.optionNumber}
            onChange={(e) => {
              setFormData({ ...formData, optionNumber: Number(e.target.value) });
              setErrors({ ...errors, optionNumber: '' });
            }}
            required
            min={0}
            max={99}
          />
          {errors.optionNumber && (
            <p className="mt-1 text-sm text-red-600">{errors.optionNumber}</p>
          )}
          <p className="mt-1 text-sm text-gray-500">Number between 0 and 99</p>
        </div>
        
        <div>
          <Input
            label="Option Name"
            value={formData.optionName}
            onChange={(e) => {
              setFormData({ ...formData, optionName: e.target.value });
              setErrors({ ...errors, optionName: '' });
            }}
            required
            maxLength={35}
          />
          {errors.optionName && (
            <p className="mt-1 text-sm text-red-600">{errors.optionName}</p>
          )}
          <p className="mt-1 text-sm text-gray-500">Maximum 35 characters</p>
        </div>
        
        <div>
          <Input
            label="Program Name"
            value={formData.programName}
            onChange={(e) => {
              setFormData({ ...formData, programName: e.target.value.toUpperCase() });
              setErrors({ ...errors, programName: '' });
            }}
            required
            maxLength={8}
          />
          {errors.programName && (
            <p className="mt-1 text-sm text-red-600">{errors.programName}</p>
          )}
          <p className="mt-1 text-sm text-gray-500">Maximum 8 characters</p>
        </div>
        
        <div>
          <Input
            label="Admin User ID"
            value={formData.adminUserId}
            onChange={(e) => {
              setFormData({ ...formData, adminUserId: e.target.value.toUpperCase() });
              setErrors({ ...errors, adminUserId: '' });
            }}
            required
            maxLength={8}
          />
          {errors.adminUserId && (
            <p className="mt-1 text-sm text-red-600">{errors.adminUserId}</p>
          )}
          <p className="mt-1 text-sm text-gray-500">Maximum 8 characters</p>
        </div>
        
        <div className="flex items-center">
          <input
            type="checkbox"
            id="isActive"
            checked={formData.isActive}
            onChange={(e) => setFormData({ ...formData, isActive: e.target.checked })}
            className="h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
          />
          <label htmlFor="isActive" className="ml-2 block text-sm text-gray-900">
            Active (uncheck for "Coming Soon" status)
          </label>
        </div>
        
        <div className="flex gap-2 pt-4">
          <Button type="submit" disabled={loading}>
            {loading ? 'Creating...' : 'Create Menu Option'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push('/admin/menu-options')}
          >
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
