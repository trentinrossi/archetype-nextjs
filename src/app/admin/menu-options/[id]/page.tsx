'use client';

import React, { useEffect, useState } from 'react';
import { useParams, useRouter } from 'next/navigation';
import { adminMenuOptionService } from '@/services/adminMenuOptionService';
import { AdminMenuOption } from '@/types/admin-menu-option';
import { Button } from '@/components/ui';

export default function AdminMenuOptionDetailPage() {
  const params = useParams();
  const router = useRouter();
  const [menuOption, setMenuOption] = useState<AdminMenuOption | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    if (params.id) {
      fetchMenuOption(Number(params.id));
    }
  }, [params.id]);

  const fetchMenuOption = async (id: number) => {
    try {
      const data = await adminMenuOptionService.getAdminMenuOptionById(id);
      setMenuOption(data);
    } catch (err) {
      console.error('Failed to load menu option:', err);
    } finally {
      setLoading(false);
    }
  };

  const handleDelete = async () => {
    if (!confirm('Are you sure you want to delete this menu option?')) return;
    
    try {
      await adminMenuOptionService.deleteAdminMenuOption(Number(params.id));
      router.push('/admin/menu-options');
    } catch (err) {
      alert('Failed to delete menu option');
      console.error(err);
    }
  };

  const handleToggleActive = async () => {
    if (!menuOption) return;
    
    try {
      if (menuOption.isActive) {
        await adminMenuOptionService.deactivateAdminMenuOption(menuOption.id);
      } else {
        await adminMenuOptionService.activateAdminMenuOption(menuOption.id);
      }
      fetchMenuOption(Number(params.id));
    } catch (err) {
      alert('Failed to toggle menu option status');
      console.error(err);
    }
  };

  if (loading) return <div className="p-6">Loading...</div>;
  if (!menuOption) return <div className="p-6">Menu option not found</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <h1 className="text-2xl font-bold">Menu Option Details</h1>
        <div className="flex gap-2">
          <Button onClick={() => router.push(`/admin/menu-options/${menuOption.id}/edit`)}>
            Edit
          </Button>
          <Button variant="secondary" onClick={handleToggleActive}>
            {menuOption.isActive ? 'Deactivate' : 'Activate'}
          </Button>
          <Button variant="danger" onClick={handleDelete}>
            Delete
          </Button>
          <Button variant="secondary" onClick={() => router.push('/admin/menu-options')}>
            Back to List
          </Button>
        </div>
      </div>
      
      <div className="bg-white shadow rounded-lg p-6 space-y-4">
        <div>
          <label className="block text-sm font-semibold text-gray-700">Option Number</label>
          <p className="mt-1 text-gray-900 text-lg font-mono">
            {menuOption.optionNumber.toString().padStart(2, '0')}
          </p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">Option Name</label>
          <p className="mt-1 text-gray-900 text-lg">{menuOption.optionName}</p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">Program Name</label>
          <p className="mt-1 text-gray-900 font-mono">{menuOption.programName}</p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">Status</label>
          <p className="mt-1">
            <span className={`px-3 py-1 rounded-full text-sm font-semibold ${
              menuOption.isActive 
                ? 'bg-green-100 text-green-800' 
                : 'bg-yellow-100 text-yellow-800'
            }`}>
              {menuOption.statusDisplay}
            </span>
          </p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">Admin User ID</label>
          <p className="mt-1 text-gray-900 font-mono">{menuOption.adminUserId}</p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">Option Display</label>
          <p className="mt-1 text-gray-900">{menuOption.optionDisplay}</p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">Created At</label>
          <p className="mt-1 text-gray-900">{new Date(menuOption.createdAt).toLocaleString()}</p>
        </div>
        
        <div>
          <label className="block text-sm font-semibold text-gray-700">Updated At</label>
          <p className="mt-1 text-gray-900">{new Date(menuOption.updatedAt).toLocaleString()}</p>
        </div>
      </div>
    </div>
  );
}
