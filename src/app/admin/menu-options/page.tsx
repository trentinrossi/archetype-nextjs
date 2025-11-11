'use client';

import React, { useEffect, useState, useCallback } from 'react';
import { useRouter } from 'next/navigation';
import { adminMenuOptionService } from '@/services/adminMenuOptionService';
import { AdminMenuOption } from '@/types/admin-menu-option';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell, Button } from '@/components/ui';

export default function AdminMenuOptionsPage() {
  const router = useRouter();
  const [menuOptions, setMenuOptions] = useState<AdminMenuOption[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  const fetchMenuOptions = useCallback(async () => {
    try {
      setLoading(true);
      const response = await adminMenuOptionService.getAdminMenuOptions(0, 100);
      setMenuOptions(response.content || response);
      setError(null);
    } catch (err) {
      setError('Failed to load admin menu options');
      console.error(err);
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchMenuOptions();
  }, [fetchMenuOptions]);

  const handleDelete = async (id: number) => {
    if (!confirm('Are you sure you want to delete this menu option?')) return;
    
    try {
      await adminMenuOptionService.deleteAdminMenuOption(id);
      fetchMenuOptions();
    } catch (err) {
      alert('Failed to delete menu option');
      console.error(err);
    }
  };

  const handleToggleActive = async (option: AdminMenuOption) => {
    try {
      if (option.isActive) {
        await adminMenuOptionService.deactivateAdminMenuOption(option.id);
      } else {
        await adminMenuOptionService.activateAdminMenuOption(option.id);
      }
      fetchMenuOptions();
    } catch (err) {
      alert('Failed to toggle menu option status');
      console.error(err);
    }
  };

  if (loading) return <div className="p-6">Loading...</div>;
  if (error) return <div className="p-6 text-red-600">Error: {error}</div>;

  return (
    <div className="p-6">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold">Admin Menu Options</h1>
          <p className="text-gray-600 mt-1">Manage administrative menu options and their availability</p>
        </div>
        <div className="flex gap-2">
          <Button onClick={() => router.push('/admin/menu-options/new')}>
            Create Menu Option
          </Button>
          <Button variant="secondary" onClick={() => router.push('/admin/menu')}>
            Back to Menu
          </Button>
        </div>
      </div>
      
      <Table>
        <TableHeader>
          <TableRow>
            <TableHead>Option #</TableHead>
            <TableHead>Option Name</TableHead>
            <TableHead>Program Name</TableHead>
            <TableHead>Status</TableHead>
            <TableHead>Admin User</TableHead>
            <TableHead>Actions</TableHead>
          </TableRow>
        </TableHeader>
        <TableBody>
          {menuOptions.length === 0 ? (
            <TableRow>
              <TableCell colSpan={6} className="text-center text-gray-500 py-8">
                No menu options found
              </TableCell>
            </TableRow>
          ) : (
            menuOptions.map((option) => (
              <TableRow key={option.id}>
                <TableCell>
                  <div className="cursor-pointer font-mono font-semibold" onClick={() => router.push(`/admin/menu-options/${option.id}`)}>
                    {option.optionNumber.toString().padStart(2, '0')}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer" onClick={() => router.push(`/admin/menu-options/${option.id}`)}>
                    {option.optionName}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer font-mono" onClick={() => router.push(`/admin/menu-options/${option.id}`)}>
                    {option.programName}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer" onClick={() => router.push(`/admin/menu-options/${option.id}`)}>
                    <span className={`px-3 py-1 rounded-full text-xs font-semibold ${
                      option.isActive 
                        ? 'bg-green-100 text-green-800' 
                        : 'bg-yellow-100 text-yellow-800'
                    }`}>
                      {option.statusDisplay}
                    </span>
                  </div>
                </TableCell>
                <TableCell>
                  <div className="cursor-pointer font-mono" onClick={() => router.push(`/admin/menu-options/${option.id}`)}>
                    {option.adminUserId}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="flex gap-2">
                    <Button 
                      size="sm" 
                      onClick={(e) => {
                        e.stopPropagation();
                        router.push(`/admin/menu-options/${option.id}/edit`);
                      }}
                    >
                      Edit
                    </Button>
                    <Button 
                      size="sm" 
                      variant="secondary"
                      onClick={(e) => {
                        e.stopPropagation();
                        handleToggleActive(option);
                      }}
                    >
                      {option.isActive ? 'Deactivate' : 'Activate'}
                    </Button>
                    <Button 
                      size="sm" 
                      variant="danger" 
                      onClick={(e) => {
                        e.stopPropagation();
                        handleDelete(option.id);
                      }}
                    >
                      Delete
                    </Button>
                  </div>
                </TableCell>
              </TableRow>
            ))
          )}
        </TableBody>
      </Table>
    </div>
  );
}
