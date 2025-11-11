'use client';

import React, { useState } from 'react';
import { useRouter } from 'next/navigation';
import { adminUserService } from '@/services/adminUserService';
import { CreateAdminUserRequest } from '@/types/admin-user';
import { Input, Button } from '@/components/ui';

export default function CreateAdminUserPage() {
  const router = useRouter();
  const [formData, setFormData] = useState<CreateAdminUserRequest>({
    userId: '',
    authenticationStatus: false,
  });
  const [loading, setLoading] = useState(false);
  const [errors, setErrors] = useState<Record<string, string>>({});

  const validateForm = (): boolean => {
    const newErrors: Record<string, string> = {};

    if (!formData.userId.trim()) {
      newErrors.userId = 'User ID is required';
    } else if (formData.userId.length > 8) {
      newErrors.userId = 'User ID must be 8 characters or less';
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
      await adminUserService.createAdminUser(formData);
      router.push('/admin/users');
    } catch (err) {
      alert('Failed to create admin user. User ID may already exist.');
      console.error(err);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="p-6 max-w-2xl">
      <h1 className="text-2xl font-bold mb-6">Create New Admin User</h1>
      
      <form onSubmit={handleSubmit} className="space-y-4">
        <div>
          <Input
            label="User ID"
            value={formData.userId}
            onChange={(e) => {
              setFormData({ ...formData, userId: e.target.value.toUpperCase() });
              setErrors({ ...errors, userId: '' });
            }}
            required
            maxLength={8}
          />
          {errors.userId && (
            <p className="mt-1 text-sm text-red-600">{errors.userId}</p>
          )}
          <p className="mt-1 text-sm text-gray-500">Maximum 8 characters</p>
        </div>
        
        <div className="flex items-center">
          <input
            type="checkbox"
            id="authenticationStatus"
            checked={formData.authenticationStatus}
            onChange={(e) => setFormData({ ...formData, authenticationStatus: e.target.checked })}
            className="h-4 w-4 text-blue-600 focus:ring-blue-500 border-gray-300 rounded"
          />
          <label htmlFor="authenticationStatus" className="ml-2 block text-sm text-gray-900">
            Authenticated
          </label>
        </div>
        
        <div className="flex gap-2 pt-4">
          <Button type="submit" disabled={loading}>
            {loading ? 'Creating...' : 'Create Admin User'}
          </Button>
          <Button
            type="button"
            variant="secondary"
            onClick={() => router.push('/admin/users')}
          >
            Cancel
          </Button>
        </div>
      </form>
    </div>
  );
}
